;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define (clojure/run code)
  (log-trace "Will run clojure code:" code)
  (j "clojure.lang.Compiler.load(new StringReader(s));" `((s ,(->jstring code)))))

(define clojure/repl-start
  (lambda* ((port 5000) (tty-port (+ port 1)))
      (clojure/run
       (string-append
        "(use '[clojure.tools.nrepl.server :only (start-server stop-server)])"       
        "(start-server :port "  (number->string port) ")"
        "(require '[clojure.tools.nrepl.transport :as t])"
        "(start-server :transport-fn t/tty :port " (number->string tty-port)  ")"
        ))))

;;
;; clj-map is something like (clj "{:key1 1 :key2 2}") and key is a string or symbol without the ":"
;; example: (->scm-object (clojure/find-value-by-key (clj "{:key1 1 :key2 2}") 'key2))
;; will return 2
;;
(define (clojure/find-value-by-key clj-map key)
  (clj "(val (find cljmap (keyword k)))" `((cljmap ,clj-map) (k ,(->jstring key)))))

(define (symbol->clj-keyword symbol)
  ((generic-java-method 'intern)
   (java-null
    (java-class '|clojure.lang.Keyword|)) (->jstring symbol)))

(define (symbol->clj-symbol symbol)
  (j "clojure.lang.Symbol.create(symbol)" `((symbol ,(->jstring symbol)))))

;;
;; BE CAREFUL HERE: a keyword is composed by namespace and name (see clojure spec).
;; This function simply drop the keyword namespace (and also the :),
;; so (clj-keyword->symbol (symbol->clj-keyword 'namespace/name))
;; is only 'name.
;;
;; Note: If you want the string representation of a keyword (usually something like :namespace/name)
;; use clj-keyword->string
;;
(define (clj-keyword->symbol keyword)
  (->symbol (j "keyword.getName()" `((keyword ,keyword)))))

;;
;; Different of clj-keyword->symbol it will keep the namespace AND the : in front of string.
;;
(define (clj-keyword->string keyword)
  (->string keyword))

;;
;; another way to convert clojure.lang.Ratio into scheme exact number:
;; (/ (->scm-object (j "n.numerator" `((n ,clojure-ratio)))) (->scm-object (j "n.denominator" `((n ,clojure-ratio))))))
(define (clj-number->number clojure-number)
  (string->number (->string clojure-number)))

;;
;; to extract numerator and denominator of ANY type of clojure number (the result of this function)
;; use like this: (clj "(numerator (clojure.lang.Numbers/toRatio (rationalize a)))" `((a ,(number->clj-number 3323/123))))
;;                (clj "(denominator (clojure.lang.Numbers/toRatio (rationalize a)))" `((a ,(number->clj-number 3323/123))))
;;
;; results in 3323 and 123. See why this is necessary:
;; https://stackoverflow.com/questions/25194809/how-to-convert-a-number-to-a-clojure-lang-ratio-type-in-clojure
;;
(define (number->clj-number scm-number)
  (let ((scm-number (inexact->exact scm-number)))
    (clj "(/ a b)" `((a ,(integer->jbigint (numerator scm-number)))
                     (b ,(integer->jbigint (denominator scm-number)))))))

(define (list->persistent-vector scheme-list)
  (j "clojure.lang.PersistentVector.create(lst);" `((lst ,(jlist->jarray (->jobject scheme-list))))))

;;
;; This alist is like `((a "b") (c "d"))
;;
(define (alist->persistent-map scheme-alist)
  (j "clojure.lang.PersistentArrayMap.create(themap);" `((themap ,(->jmap scheme-alist)))))

(define (persistent-vector-size pv)
  (->number (j "pv.count();" `((pv ,pv)))))

(define (get-iterator-from-persistent-hash-set phs)
  ((generic-java-method '|iterator|) phs))

(define (persistent-hash-set->jarray phs)
  ((generic-java-method '|toArray|) phs))

(define* (map-persistent-vector fn pv
                                (start: start 0)
                                (end: end #f))
  (let* ((size (persistent-vector-size pv))
         (start (max (min start (- size 1)) 0))
         (end (min (or end size) size)))
    (list-ec (: i start end)
             (fn (j "pv.nth(iii);" `((pv ,pv)
                                     (iii ,(->jint i))))))))

(define (only-persistent-vector pv)
  (assert (= 1 (persistent-vector-size pv)))
  ((generic-java-method '|nth|) pv (->jint 0)))

(define (get-persistent-vector pv index)
  ((generic-java-method '|nth|) pv (->jint index)))

(define (get-keys-persistent-hash-map phm)
  (->scm-object (j "phm.keySet();" `((phm ,phm)))))

(define* (get-value-persistent-hash-map phm key
					(default-value: default-value #f)
                                        (convert-result: convert-result #t))
  (let* ((key (if (symbol? key)
                  (symbol->clj-keyword key)
                key))
	 (default (->jstring "80414c52-346f-47e3-b775-f2a5d5556422"))
	 (result ((generic-java-method '|valAt|) phm key default)))
    (if (->boolean ((generic-java-method '|equals|) default result))
	default-value
        (if convert-result
            (->scm-object result)
            result))))

(define (create-runner)
  (j
 "
   import java.io.StringReader; import java.util.HashMap; import java.util.Map; import java.util.Map.Entry; import clojure.lang.Associative; import clojure.lang.Binding; import clojure.lang.Compiler; import clojure.lang.Namespace; import clojure.lang.PersistentHashMap; import clojure.lang.RT; import clojure.lang.Symbol; import clojure.lang.Var;

   public class ClojureScriptRunner {

       public static Object runClosureScript(Map bindings, String script) throws Exception {
           Object ClojureScriptRunner_result=null;

	   try { 
	       new Binding(script); Namespace ns = (Namespace) RT.CURRENT_NS.get(); Associative mappings = PersistentHashMap.EMPTY; 
	       mappings = mappings.assoc(RT.CURRENT_NS, RT.CURRENT_NS.get()); 
	       for (Entry e : bindings.entrySet()) { 
		   String varName = e.getKey(); Symbol sym = Symbol.intern(varName); Var var = Var.intern(ns, sym); 
		   mappings = mappings.assoc(var, e.getValue()); 
	       }
               Var.pushThreadBindings(mappings);
               ClojureScriptRunner_result = Compiler.load(new StringReader(script));
	   } finally { 
	       try {
                   Var.popThreadBindings();
               } catch(Exception e) {
                   throw new RuntimeException(\"Exception while running Clojure script.\", e);
               }
           }

           return ClojureScriptRunner_result;
       }

       
       public static void main(String[] args) throws Exception { 
	   Map bindings = new java.util.concurrent.ConcurrentHashMap(); bindings.put(\"a\", 2); bindings.put(\"b\", 3); 
	   System.out.println(runClosureScript(bindings, \"(+ a b)\"));
       }
 }"))
;;(j "ClojureScriptRunner.runClosureScript(new java.util.concurrent.ConcurrentHashMap(), \"(list (+ 3 4))\");")

(define create-runner-memoized)

(define clj)

(set! create-runner-memoized (memoize create-runner))



(set! clj
      (lambda* (str (vars #f))
               (mutex/synchronize (mutex-of create-runner) (lambda () (create-runner)))
               (let ((result #f))
                 (cond ( (eqv? vars #t) ; This is not a mistake. It could be a list too - and we should not enter this code.
                         (begin
                           (error "\nFetching full environment not yet supported.\n")) )
                       (  (eqv? vars #f)
                          (let ((strvar (random-var)))
                            (set! result (j (format "ClojureScriptRunner.runClosureScript(new java.util.concurrent.ConcurrentHashMap(), ~a);" strvar)
                                            `((,strvar ,(->jstring str)))))) )
                       [ (list? vars)

                         (let* ((random-string-to-avoid-thread-conflict (number->string (random)))
                                (clj-let-declaration (reduce string-append* ""
                                                             (map (lambda (key-value-pair)
                                                                    (let ((java-map-key (->jstring (string-append*
                                                                                                    (car key-value-pair)
                                                                                                    random-string-to-avoid-thread-conflict))))
                                                                      (j "iu.M.d.put(k, v);"
                                                                         `((k ,java-map-key)
                                                                           (v ,(cadr key-value-pair))))

                                                                      (string-append* " "
                                                                                      (car key-value-pair)
                                                                                      " "
                                                                                      "(.get iu.M/d \"" (->string java-map-key) "\")")))
                                                                  vars)))

                                (final-command (string-append* "(let ["
                                                               clj-let-declaration
                                                               "] " str ")")))

                           (let ((finalcommand-var (random-var)))
                             (set! result (j (format "ClojureScriptRunner.runClosureScript(new java.util.concurrent.ConcurrentHashMap(), ~a);" finalcommand-var)
                                             `((,finalcommand-var ,(->jstring final-command))))))

                           ; clear memory
                           (for-each (lambda (key-value-pair)
                                       (j "iu.M.d.remove(k);" `((k ,(->jstring (string-append*
                                                                                (car key-value-pair)
                                                                                random-string-to-avoid-thread-conflict))))))
                                     vars))

                         ])

                 result)))
