(define (js-manager)  (j "new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");"))

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
                      (j "manager.put(jsobjectname, jsobjectvalue);"
                         `((manager ,manager)
                           (jsobjectname ,(->jstring sname))
                           (jsobjectvalue ,(->jobject vvalue))))))))
     vars))
  
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

;; Example: (run-js/s (js-manager) "var f = function (what) { return 'hello, ' + what; }; f('Javascript');")


