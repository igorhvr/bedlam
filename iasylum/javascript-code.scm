(define-generic-java-method get |get|)
(define-generic-java-method set |set|)

;;nodejs - remote
(define (nodejs-global code)
  (log-trace "About to execute js code" code)
  (let ((result (http-call-post-string/string "http://js:27429/" code)))
    (log-trace "Result" result)
    (if (and result
             (not (string=? "" result)))
        (json->scheme result)
        #f)))

(define* (nodejs code (vars '()))
  (let* ((names-and-values-pair (call-with-values
                                    (lambda () (unzip2 vars))
                                  (lambda (names values) (cons names values))))
         (names (car names-and-values-pair))
         (values (cdr names-and-values-pair))
         (code (format "(function(~a) { return ~a })(~a);"
                       (apply add-between (cons "," names))
                       code
                       (apply add-between (cons "," (map (lambda (v)
                                                           (cond [(number? v)
                                                                  (number->string v)]
                                                                 [else
                                                                  (string-append* "\"" v "\"")]))
                                                         values))))))
    (node-js-global code)))

;; generic
(define js)
(define get-local-javascript-manager)
(define create-thread-local-javascript-manager-retriever)
(define js-manager)

;; rhino
(define (rhino-create-thread-local-javascript-manager-retriever)
    (let ((tl
           (j "mtl=new ThreadLocal() {
                   protected synchronized Object initialValue() {                       
                       return new javax.script.ScriptEngineManager().getEngineByName(\"rhino\");
                   }
               }; mtl;")))
      (lambda ()
        (let ((result (get tl)))
          result))))

(define rhino-get-local-javascript-manager)
(define js-rhino)
(define (rhino-js-manager)  (j "new javax.script.ScriptEngineManager().getEngineByName(\"rhino\");"))


;; v8
(define v8-get-local-javascript-manager)
(define js-v8)
(define (v8-create-thread-local-javascript-manager-retriever)
    (let ((tl
           (j "mtlv8=new ThreadLocal() {
                   protected synchronized Object initialValue() {                       
                       return com.eclipsesource.v8.V8.createV8Runtime();
                       //return \"<for-v8-no-js-manager-because-we-are-single-threaded>\";
                   }
               }; mtlv8;")))
      (lambda ()
        (let ((result (get tl)))
          result))))

(define v8-javascript-work-queue)

(define (v8-js-manager) (j "return com.eclipsesource.v8.V8.createV8Runtime();"))
;; (define (v8-js-manager) "<for-v8-no-js-manager-because-we-are-single-threaded>")

(define (syserr-log p) (j "System.err.print(m); System.err.flush();" `((m ,(->jobject p)))))

(define javascript-processing-thread-error-handler
    (lambda (error error-continuation)
      (map syserr-log `("Erro while processing javascript code  - will NOT stop thread "  ,(->string (j "Thread.currentThread().getName();")) ": " ,error ,error-continuation "\n"))
      (print-stack-trace error-continuation)))

(define standard-failure (make-error 'v8-javascript-execution-failure-see-stderr-logs-for-full-details))

(define fixed-v8-runtime (make-parameter* #f))

;; rhino
(set! rhino-get-local-javascript-manager (rhino-create-thread-local-javascript-manager-retriever))
(set! js-rhino
  (lambda*
   (code (vars #f) (manager (rhino-get-local-javascript-manager)))
   ;(d/n "Will run code: (((((" code "))))) saved to " (save-to-somewhere code))
   ;(d/n "vars: " vars)
   (j                       
       "import javax.script.*;
        cx = org.mozilla.javascript.Context.enter();
        cx.setOptimizationLevel(-1);"        
       `((manager ,manager)))
  
  (when vars
    (for-each
     (lambda (v)
       (match-let ( ( (vname vvalue) v ) )
                  (begin
                    (let* ((sname (if (string? vname) vname (symbol->string vname)))
                           (name (->jstring sname )))
                      ;(d/n "saving " sname " to " (save-to-somewhere vvalue))
                      (j "manager.put(jsobjectname, jsobjectvalue);"
                         `((manager ,manager)
                           (jsobjectname ,(->jstring sname))
                           (jsobjectvalue ,(->jobject vvalue))))))))
     vars))

  (let ((final-result
   (j                       
       "ourresult=null;
        try {           
           jso=manager.eval(code);
           if(jso!=null) ourresult=jso;
        } finally {
           cx.exit();
        }
        ourresult;"
       `((manager ,manager) (code ,(->jstring code))))))
    ;(d/n "final-result: " (save-to-somewhere final-result))
    final-result
    )))

(set! v8-get-local-javascript-manager (v8-create-thread-local-javascript-manager-retriever))

;;(set! v8-javascript-work-queue (make-queue 1))

;;(start-worker
;; (lambda (o)
;;   (with/fc
;;    javascript-processing-thread-error-handler
;;    o))
;; v8-javascript-work-queue
;;'log-trace-execution: (make-parameter #f))

(set! js-v8
      (lambda*
       (code (vars #f)
             ;;(runtime 'ignored)
             (runtime (v8-get-local-javascript-manager))
             )
       
       (let ((response-queue (make-queue 1)))
         (let* (
                (thunk-to-execute
                 (lambda ()
                   (let ((runtime ;;(or (fixed-v8-runtime) (begin (fixed-v8-runtime (j "return com.eclipsesource.v8.V8.createV8Runtime();")) (fixed-v8-runtime)))
                                  runtime
                                  ))
                    (response-queue 'put
                      (let ((vars (or vars '())))
                        (each-for
                         vars
                         (lambda (v)
                           (match-let ( ( (vname vvalue) v ) )
                                      (begin
                                        (let* ((sname (if (string? vname) vname (symbol->string vname)))
                                               (name (->jstring sname )) (code-return (random-var))
                                               (f-p (random-var)) (code (string-append*
                                              "var " sname " = null; var fdefdeff" code-return " = function(" f-p ") { " sname " = " f-p "; }; fdefdeff" code-return ";"))
                                               (v8f (j "v8.executeScript(script);" `((v8 ,runtime) (script ,(->jstring code)))))
                                               (v8array-return (random-var))
                                               (parameters-v8array (j (string-append
                                                                       "tres" v8array-return  "=new com.eclipsesource.v8.V8Array(v8runtime); tres" v8array-return ";")
                                                                      `((v8runtime ,runtime)))))
                                          (j "v8a.push(jsobjectvalue);"
                                             `((v8a ,parameters-v8array)
                                               (jsobjectvalue ,(->jobject vvalue))))
                                          (j "v8f.call(null, p);" `((p ,parameters-v8array) (v8f ,v8f)))
                                          (j (string-append "fdefdeff" code-return " = null;"))  (j (string-append "tres" v8array-return " = null;"))
                                          )))))
                        (let ((final-result
                               ;;(try-and-if-it-fails-object (standard-failure)
                                                           (j "v8.executeScript(script);" `((v8 ,runtime) (script ,(->jstring code))))
                               ;;                            )
                               ))
                          (each-for vars
                                    (lambda (v)
                                      (match-let ( ( (vname vvalue) v ) )
                                                 (let* ((sname (if (string? vname) vname (symbol->string vname)))
                                                        (name (->jstring sname )))
                                                   (let ((cleanup-code (string-append sname " = null;")))
                                                     (j "v8.executeScript(script);" `((v8 ,runtime) (script ,(->jstring cleanup-code)))))
                                                   ))))
                          final-result
                          )))))))
           ;;(v8-javascript-work-queue 'put thunk-to-execute)
           (thunk-to-execute)
           )
         (let ((return-or-raise (response-queue 'take-java)))
           ;;(if (string=? (->string (j "fr.getClass().getName();" `((fr ,return-or-raise)))) "com.eclipsesource.v8.V8Object")
               ;;(throw (make-error "v8 object would be returned outside v8 context causing crash."))
               return-or-raise
               ;;)
           ))))
            
(set! get-local-javascript-manager v8-get-local-javascript-manager)
(set! create-thread-local-javascript-manager-retriever v8-create-thread-local-javascript-manager-retriever)
(set! js-manager v8-js-manager)
(set! js js-v8)

