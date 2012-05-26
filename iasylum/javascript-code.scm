(define (js-manager)
  (j "org.apache.bsf.BSFManager manager = new org.apache.bsf.BSFManager();   manager;"))

(define (run-js/s manager code)
  (->string 
   (j "Object jso = manager.eval(\"javascript\", \"(java)\", 1, 1, code);
      jso.toString();" `((manager ,manager) (code ,(->jstring code))))))

;; Example: (run-js/s (js-manager) "var f = function (what) { return 'hello, ' + what; }; f('Javascript');")


