(define (js-manager)  (j "new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");"))

(define (run-js/s manager code . vars)
  (j                       
       "import javax.script.*;
        cx = org.mozilla.javascript.Context.enter();
        cx.setOptimizationLevel(-1);
        ourresult=\"\";"        
       `((manager ,manager)))
  
  (when (= (length vars) 1)
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
     (car vars)))
  
  (->string 
   (j                       
       "import javax.script.*;
        cx = org.mozilla.javascript.Context.enter();
        cx.setOptimizationLevel(-1);
        ourresult=\"\";
        try {           
           jso=manager.eval(code);
           if(jso!=null) ourresult=jso.toString();
        } finally {
           cx.exit();
        }
        ourresult;"
       `((manager ,manager) (code ,(->jstring code))))))

;; Example: (run-js/s (js-manager) "var f = function (what) { return 'hello, ' + what; }; f('Javascript');")


