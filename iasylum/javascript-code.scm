(define (nodejs code)
  (try-and-if-it-throws-object
   (#f)
   (json->scheme (http-call-post-string/string "http://js:27429/" code))))

(define (js-manager)  (j "new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");"))

(define-generic-java-method get |get|)
(define-generic-java-method set |set|)

(define (create-thread-local-javascript-manager-retriever)
    (let ((tl
           (j "mtl=new ThreadLocal() {
                   protected synchronized Object initialValue() {                       
                       return new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");
                   }
               }; mtl;")))
      (lambda ()
        (let ((result (get tl)))
          result))))

(define get-local-javascript-manager)
(define js)

(set! get-local-javascript-manager (create-thread-local-javascript-manager-retriever))

(set! js
  (lambda*
   (code (vars #f) (manager (get-local-javascript-manager)))
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

;; Example: (run-js/s (js-manager) "var f = function (what) { return 'hello, ' + what; }; f('Javascript');")
