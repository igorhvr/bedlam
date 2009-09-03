;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define-java-class |bsh.Interpreter|)
(define-generic-java-method |eval|)
(define-generic-java-method |set|)

(define (->jobject v)
  (cond
   ((boolean? v) (->jboolean v))
   ((char? v) (->jchar v))
   ((string? v) (->jstring v))
   ((integer? v) (if (or (< v -2147483648) (> v 2147483647)) (->jlong v) (->jint v)))
   ((number? v) (->jdouble v))
   (else  (java-wrap v)) ; Ok. I give up.
   ))

(define j)
(define tint)

(set! tint
      (begin
        (let ((tint (java-new |bsh.Interpreter|)))
          (|eval| tint
                  (->jstring
                   (string-append "s(cmd_str) {
                                              java.lang.Object siscInterpreter = sisc.interpreter.Context.enter();
                                              java.lang.Object result=siscInterpreter.eval(cmd_str);
                                              sisc.interpreter.Context.exit();
                                              return result;
                                          } ")))
          (|eval| tint
                         (->jstring
                          (string-append "s(cmd_str) {
                                              java.lang.Object siscInterpreter = sisc.interpreter.Context.enter();
                                              java.lang.Object result=siscInterpreter.eval(cmd_str);
                                              sisc.interpreter.Context.exit();
                                              return result;
                                          } ")))
          tint)))
        
      
(set! j
      (lambda* (str (vars #f))
               (let ((tint tint))
                 (cond ( (eqv? vars #t) ; This is not a mistake. It could be a list too - and we should not enter this code.
                         (begin
                           (let ((env-data1 (interaction-environment))
                                 (env-data2 (get-sidecar-environment '*sc-expander* (interaction-environment))))
                             (|set| tint (->jstring "JCODE_2489234982_jcodeSiscEnvironmentData_1") (java-wrap env-data1))
                             (|set| tint (->jstring "JCODE_2489234982_jcodeSiscEnvironmentData_2") (java-wrap env-data2)))
                           
                           (|eval| tint
                                   (->jstring
                                    "JCODE_2489234982_currentSymbolName=null;
                                     JCODE_2489234982_v=new sisc.util.ExpressionVisitor() {
                                   boolean visit(sisc.util.ExpressionVisitee e) {
                                     if(e instanceof sisc.data.MemoizedSymbol) {
                                         JCODE_2489234982_currentSymbolName=e.toString();
                                     } else {
                                        this.interpreter.set(JCODE_2489234982_currentSymbolName,e);
                                     }
                                     return true;                            
                                 }
                             };
                             JCODE_2489234982_jcodeSiscEnvironmentData_2.visit(JCODE_2489234982_v);
                             JCODE_2489234982_jcodeSiscEnvironmentData_1.visit(JCODE_2489234982_v);"
                                    ))) )
                       ( (list? vars)
                         (begin
                           (map (lambda (v)
                                   (match-let ( ( (vname vvalue) v ) )
                                              (|set| tint (->jstring (if (string? vname) vname (symbol->string vname)))
;                                                     TODO: The below is too magical. I need to find a better way.                                                     
;                                                     (cond
;                                                      ((boolean? vvalue) (->jboolean vvalue))
;                                                      ((char? vvalue) (->jchar vvalue))
;                                                      ((string? vvalue) (->jstring vvalue))
;                                                      (else  vvalue))
                                                     vvalue
                                                     )))

                                vars))))                                           
                 (|eval| tint (->jstring str)))))


; TODO: Understand why (integer? 32833333333333333333333.222222233) = #t

;(j "1+1;")
; Test case: (j "s(\"(+ 1 (->number (j \\\"1+3;\\\")))\").longValue();")
;
;(j "Class.forName(\"java.util.HashMap\");")
;(define to_concat1 "fklsdl")
;(define to_concat2 "fldsfls")
;(define genstr (lambda (p) (string-append "\n" p "\n")))
;(define param "someparam")
;(j "param")
;(->string (j "param"))
;(->string (j "s(\"(genstr param)\") + to_concat1.toString() + to_concat2.toString() + to_concat1.getClass().getName().toString();"))
;(j "1+1;")
;
