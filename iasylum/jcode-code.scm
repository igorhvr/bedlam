;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define (_n_ o i) (java-array-ref o i))
(define (_0_ o) (java-array-ref o 0))
(define (_1_ o) (java-array-ref o 1))
(define (_2_ o) (java-array-ref o 2))
(define (_3_ o) (java-array-ref o 3))
(define (_4_ o) (java-array-ref o 3))



(define-java-class |bsh.Interpreter|)


(define-generic-java-method |eval|)
(define-generic-java-method |set|)

(define-java-class |iu.ThreadLocalVariableManager|)
(define-generic-java-method get-bsh-interpreter)
(define-generic-java-method set-bsh-interpreter)

(define (get-local-interpreter)
  (let ((current-value (get-bsh-interpreter (java-null |iu.ThreadLocalVariableManager|))))    
    (if (java-null? current-value)
        (let ((result (java-new |bsh.Interpreter|)))          
          (set-bsh-interpreter (java-null |iu.ThreadLocalVariableManager|) result)
          (startup-interpreter result)
          result)
        current-value)))

(define (->scm-object v)
  (if (java-object? v)
      (if (java-null? v) '()
          (let ((obj-class (->string (j "v.getClass().getName();" `((v ,v))))))
            (cond
             ((string=? obj-class "java.lang.String") (->string v))
             ((or
               (string=? obj-class "java.lang.Byte")
               (string=? obj-class "java.lang.Short")
               (string=? obj-class "java.lang.Integer")
               (string=? obj-class "java.lang.Long")
               (string=? obj-class "java.lang.Float")
               (string=? obj-class "java.lang.Double")
               )
              (->number v))
             ((string=? obj-class "java.lang.Character") (->character v))
             ((->boolean (j "(tobj instanceof java.util.Date);" `((tobj ,v))) ) (jdate->date v))
             ((instance-of v "java.util.Collection")
              (let ((it (j "c.iterator();" `((c ,v)))))
                (map ->scm-object (iterable->list it))))
             
             ((string=? obj-class "bsh.Primitive")
              (->scm-object (j "hhhhhhhhhh.getValue();" `((hhhhhhhhhh ,v))))
              )
             (else  (begin
                      ;(display (string-append "\n[" obj-class "]\n"))
                      (java-unwrap v)))))) ; Ok. I give up.         
          v))


  (define iterable->list
    (lambda* (o (proc (lambda (p) p)))
             (let ((result (list)))
               (define (p d)
                 (set! result (cons (proc d) result)))
               (for-each-iterable o p)
               (reverse result))))
  
  (define for-each-iterable
    (lambda (o proc)
      (define-generic-java-method iterator)
      (define-generic-java-method has-next)
      (define-generic-java-method next)
      (let ((it (if (->boolean (j "o instanceof java.util.Iterator;"  `((o ,o)))) o (iterator o))))
        (let loop ()
          (let ((more-data (->boolean (has-next it))))
            (if more-data
                (let ((e (next it)))
                  (proc e)
                  (loop))))))))
          
(define (instance-of object class)
  (if (java-object? object)
      (->boolean (j (string-append "(tobj instanceof " class ");")  `((tobj ,object))))
      #f))

(define-java-classes (<date> |java.util.Date|))

(define (date->jdate the-date)
   (let ((t (date->time-utc the-date)))
     (java-new <date> (->jlong (* 1000 (time-second t))))))

(define (jdate->date jd)
  (let ((vls (j "import java.util.Calendar;
                 TimeZone tz = TimeZone.getTimeZone(\"UTC\");
                 cal=new java.util.GregorianCalendar(tz,java.util.Locale.getDefault());
                 cal.setTime(jd);
                 new Object[]{
                 cal.get(Calendar.MILLISECOND),
                 cal.get(Calendar.SECOND),cal.get(Calendar.MINUTE),cal.get(Calendar.HOUR_OF_DAY),
                 cal.get(Calendar.DAY_OF_MONTH),cal.get(Calendar.MONTH),cal.get(Calendar.YEAR)};" `((jd ,jd)))))
    (let ((milliseconds (->number (java-array-ref vls 0)))
          (seconds (->number (java-array-ref vls 1)))
          (minutes (->number (java-array-ref vls 2)))
          (hours (->number (java-array-ref vls 3)))
          (day-of-month (->number (java-array-ref vls 4)))
          (orig-month-of-year (->number (java-array-ref vls 5)))
          (year (->number (java-array-ref vls 6))))
        (let ((month-of-year (+ 1 orig-month-of-year)))
          ;;make-date nanosecond second minute hour day month year zone-offset
           (make-date (* 1000 milliseconds) seconds minutes hours day-of-month month-of-year year 0)))))


(define (->jobject v)
  (if (java-object? v) v
      (cond
       ((boolean? v) (->jboolean v))
       ((char? v) (->jchar v))
       ((string? v) (->jstring v))
       ((integer? v) (if (or (< v -2147483648) (> v 2147483647)) (->jlong v) (->jint v)))
       ((number? v) (->jdouble v))
       ((date? v) (date->jdate v))
       (else  (java-wrap v)) ; Ok. I give up.
       )))

(define j)

(define (startup-interpreter tint)
  (|eval| tint
          (->jstring
           (string-append "s(cmd_str) {
                                              java.lang.Object siscInterpreter = sisc.interpreter.Context.enter();
                                              java.lang.Object result=siscInterpreter.eval(cmd_str);
                                              sisc.interpreter.Context.exit();
                                              return result;
                                          } "))))
        
      
(set! j
      (lambda* (str (vars #f))
               (let ((tint (get-local-interpreter)))
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
