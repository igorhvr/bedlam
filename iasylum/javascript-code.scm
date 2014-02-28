(define (js-manager)
  (j "manager = new org.apache.bsf.BSFManager();   manager;"))

(define (js-manager-no-optimization)  (j "new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");"))

(define (run-js/s manager code)
  (->string 
   (j "Object jso = manager.eval(\"javascript\", \"(java)\", 1, 1, code);
      jso.toString();" `((manager ,manager) (code ,(->jstring code))))))

(define (run-js-no-optimization/s manager code)
  (->string 
   (j                       
       "import javax.script.*;
        cx = org.mozilla.javascript.Context.enter();
        cx.setOptimizationLevel(-1);
        ourresult=\"\";
        try {
           jso=new ScriptEngineManager().getEngineByName(\"javascript\").eval(code);
           if(jso!=null) ourresult=jso.toString();
        } finally {
           cx.exit();
        }
        ourresult;"
       `((manager ,manager) (code ,(->jstring code))))))

;; Example: (run-js/s (js-manager) "var f = function (what) { return 'hello, ' + what; }; f('Javascript');")


