;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define (clojure/run code)
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
;; clj-map is something like {:key1 value1 :key2 value2} and key is a string or symbol without the ":"
;;
(define (clojure/find-value-by-key clj-map key)
  (clj "(val (find cljmap (keyword k)))" `((cljmap ,clj-map) (k ,(->jstring key)))))

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
               (create-runner)
               (let ((result #f))
                 (cond ( (eqv? vars #t) ; This is not a mistake. It could be a list too - and we should not enter this code.
                         (begin
                           (error "\nFetching full environment not yet supported.\n")) )
                       (  (eqv? vars #f)
                          (begin
                            (set! result (j "ClojureScriptRunner.runClosureScript(new java.util.concurrent.ConcurrentHashMap(), str);" `((str ,(->jstring str)))))) )
                       ( (list? vars)
                         (begin
                           (let ((parameters-map (j "params=new java.util.concurrent.ConcurrentHashMap();")))
                             (for-each
                              (lambda (v)
                                (match-let ( ( (vname vvalue) v ) )
                                           (begin
                                             (let* ((sname (if (string? vname) vname (symbol->string vname)))
                                                    (name (->jstring sname )))
                                               (j "params.put(vname, vvalue);"
                                                  `((vname ,name)
                                                    (vvalue ,vvalue)))
                                               (j "iu.M.d.put(vname, vvalue);"
                                                  `((vname ,name)
                                                    (vvalue ,vvalue)))
                                               (let ((clj-cmd (string-append " (def " sname " (.get iu.M/d \"" sname "\")) ")))
                                                 (clj clj-cmd))))))

                              
                              vars)
                             
                             ;;(set! result (j "ClojureScriptRunner.runClosureScript(params, str);" `((str ,(->jstring str)) (params ,parameters-map))))
                             (set! result (j "ClojureScriptRunner.runClosureScript(new java.util.concurrent.ConcurrentHashMap(), str);"
                                             `((str ,(->jstring str))))))) ))

                 result)))
