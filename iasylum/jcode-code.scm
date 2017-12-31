;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define (_n_ o i) (java-array-ref o i))
(define (_0_ o) (java-array-ref o 0))
(define (_1_ o) (java-array-ref o 1))
(define (_2_ o) (java-array-ref o 2))
(define (_3_ o) (java-array-ref o 3))
(define (_4_ o) (java-array-ref o 3))

(define (quote-convert s) (irregex-replace/all "'" s "\"") )

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

(define-generic-java-method to-string |toString|)

(define (->scm-object v)
  (if (java-object? v)
      (if (java-null? v) '()
          (let ((obj-class (->string (j "v.getClass().getName();" `((v ,v)))))
                (is-array? (->boolean (j "v.getClass().isArray();" `((v ,v))))))
            (cond
             (is-array? (->scm-object (jarray->jlist v)))
             ((string=? obj-class "java.lang.String") (->string v))
             ((or
               (string=? obj-class "java.lang.Byte")
               (string=? obj-class "java.lang.Short")
               (string=? obj-class "java.lang.Integer")
               (string=? obj-class "java.lang.Long")
               (string=? obj-class "java.lang.Float")
               (string=? obj-class "java.lang.Double")
               (string=? obj-class "java.math.BigInteger")
               )
              (->number v))
             ((string=? obj-class "java.lang.Character") (->character v))
             ((string=? obj-class "java.lang.Boolean") (->boolean v))
             ((string=? obj-class "java.util.UUID") (->string (to-string v)))
             ((->boolean (j "(tobj instanceof java.util.Date);" `((tobj ,v))) ) (jdate->date v))
             ((instance-of v "java.math.BigDecimal")
              (jbigdecimal->number v))
             ((instance-of v "java.util.Collection")
              (let ((it (j "c.iterator();" `((c ,v)))))
                (map ->scm-object (iterable->list it))))
             ((or (instance-of v "java.util.Set") (instance-of v "java.util.HashSet"))
              (let ((it (j "c.iterator();" `((c ,v)))))
                (map ->scm-object (iterable->list it))))
             ((instance-of v "java.util.Map")
              (let ((it (j "c.entrySet().iterator();" `((c ,v)))))
                (map ->scm-object (iterable->list it))))
             ((instance-of v "java.util.Map.Entry")
              (let ((key (j "c.getKey();" `((c ,v))))
                    (value (j "c.getValue();" `((c ,v)))))
                (cons (->scm-object key) (->scm-object value))))
             ((string=? obj-class "bsh.Primitive")
              (->scm-object (j "hhhhhhhhhh.getValue();" `((hhhhhhhhhh ,v))))
              )
             ((instance-of v "sisc.data.Value")
              (java-unwrap v))
             (else v)))) ; Ok. I give up.
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
                  (loop))))))
      (void)
      ))
          
(define (instance-of object class)
  (if (java-object? object)
      (->boolean (j (string-append "(tobj instanceof " class ");")  `((tobj ,object))))
      #f))

(define-java-classes (<date> |java.util.Date|))

(define (time-millis->jdate time-millis)
  (j "new java.util.Date(input);" `((input ,(->jlong time-millis)))))

(define (jdate->time-millis jdate)
  (j "jdate.getTime();" `((jdate ,jdate))))

(define (time->jdate t)
  (java-new <date> (->jlong (+ (time->millis t)))))

(define (date->jdate the-date)
   (let ((t (date->time-utc the-date)))
     (time->jdate t)))

(define (jdate->date jd)
  (let ((vls (j "import java.util.Calendar;
                 tz = TimeZone.getTimeZone(\"UTC\");
                 cal=new java.util.GregorianCalendar(tz,java.util.Locale.getDefault());
                 cal.setTime(jd);
                 new Object[]{
                 cal.get(Calendar.MILLISECOND),
                 cal.get(Calendar.SECOND),cal.get(Calendar.MINUTE),cal.get(Calendar.HOUR_OF_DAY),
                 cal.get(Calendar.DAY_OF_MONTH),cal.get(Calendar.MONTH),cal.get(Calendar.YEAR)};" `((jd ,jd) (cal) (tz)))))
    (let ((milliseconds (->number (java-array-ref vls 0)))
          (seconds (->number (java-array-ref vls 1)))
          (minutes (->number (java-array-ref vls 2)))
          (hours (->number (java-array-ref vls 3)))
          (day-of-month (->number (java-array-ref vls 4)))
          (orig-month-of-year (->number (java-array-ref vls 5)))
          (year (->number (java-array-ref vls 6))))
        (let ((month-of-year (+ 1 orig-month-of-year)))
          ;;make-date nanosecond second minute hour day month year zone-offset
           (make-date (* 1000000 milliseconds) seconds minutes hours day-of-month month-of-year year 0)))))

(define (alist? p)
  (every pair? p))

(define (->jobject v)
  (if (java-object? v) v
      (cond
       ((boolean? v) (->jboolean v))
       ((char? v) (->jchar v))
       ((string? v) (->jstring v))
       ((integer? v) (if (or (< v -2147483648) (> v 2147483647)) (->jlong v) (->jint v)))
       ((number? v) (->jdouble v))
       ((date? v) (date->jdate v))
       ((time? v) (time->jdate v))
       ((list? v)
        (if (and (alist? v)
                 (not (null? v)))
            (->jmap v)
            (let ((resulting-list (j "new java.util.concurrent.ConcurrentLinkedQueue();")))
              (map           
               (lambda (elementn)
                 (j "linkedlist.add(elementnx);"
                    `((elementnx ,(->jobject elementn))
                      (linkedlist ,resulting-list))))
               v)
              resulting-list)))
       ((pair? v)
        (j "new java.util.AbstractMap.SimpleEntry(first, second);"
           `((first ,(->jobject (car v)))
             (second ,(->jobject (cdr v))))))
       ((vector? v)
        (->jobject (vector->list v)))
       (else  (java-wrap v)) ; Ok. I give up.
       )))

(define (jlist->jarray l)
  (j "jlist.toArray(new Object[jlist.size()]);" `((jlist ,l))))

(define (jarray->jlist a)
  (j "Arrays.asList(a);" `((a ,a))))

(define (jarray->string a)
  (->string (j "java.util.Arrays.toString(a);" `((a ,a)))))

(define (string->juuid str)
  (j "java.util.UUID.fromString(input);" `((input ,(->jstring str)))))

(define (list->jset list)
  (j "newhs = new HashSet(); newhs.addAll(input); return newhs;"
     `((input ,(->jobject list)) (newhs))))

(define (string->jbigdecimal string)
  (j "new java.math.BigDecimal(number);" `((number ,(->jstring string)))))

(define (jbigdecimal->number jbigdecimal)
  (inexact->exact (string->number (decimal-to-fractions-inside-string
                                   (->string (j "jbig.toString();" `((jbig ,jbigdecimal))))))))

(define (string->jbigint string)
  (j "new java.math.BigInteger(number);" `((number ,(->jstring string)))))

(define* (jstream->tmp-file stream (prefix: prefix "jstream-to-file") (suffix: suffix ".tmp"))
  (j "jstreamtofile_result=java.io.File.createTempFile(prefix + \"_\",suffix);
      { 
	jstreamtofile_out = new java.io.FileOutputStream(jstreamtofile_result);
 
	jstreamtofile_read = 0;
	jstreamtofile_bytes = new byte[1024];
 
	while ((jstreamtofile_read = inputstream.read(jstreamtofile_bytes)) != -1) {
		jstreamtofile_out.write(jstreamtofile_bytes, 0, jstreamtofile_read);
	}
 	
	jstreamtofile_out.flush();
	jstreamtofile_out.close();
      }
      jstreamtofile_result;"
     `((inputstream ,stream)
       (jstreamtofile_result)
       (prefix ,(->jstring prefix))
       (suffix ,(->jstring suffix)))))

(define (java.io.InputStream->java.nio.ByteBuffer is)
  (j "java.nio.ByteBuffer.wrap(org.apache.commons.io.IOUtils.toByteArray(is));" `((is ,is))))

(define (java.io.File->java.nio.ByteBuffer file)
  (j "fin = new java.io.FileInputStream(fl);
      fchan = fin.getChannel();
      fsize = fchan.size();
      mbuf = java.nio.ByteBuffer.allocate((int) fsize);
      fchan.read(mbuf);
      mbuf.rewind();
      fchan.close(); 
      fin.close();
      mbuf;" `((fl ,(->jstring file))(fin)(fchan)(fsize))))

(define (string->java.io.InputStream s)
  (let* ((input-port (open-input-string s))
         (jreader (->jreader input-port))
         (jstream (j "new org.apache.commons.io.input.ReaderInputStream(reader);" `((reader ,jreader)))))
    jstream))

(define* (string->java.io.File s (prefix: prefix "jstream-to-file") (suffix: suffix ".tmp"))
  (jstream->tmp-file (string->java.io.InputStream s) 'prefix: prefix 'suffix: suffix))

(define (number->jbigdecimal number)
  (let ((exact-number (inexact->exact number)))
    (j "new java.math.BigDecimal(numerator).divide(new java.math.BigDecimal(denominator), java.math.MathContext.DECIMAL128);"
       `((numerator ,(->jstring (number->string (numerator exact-number))))
         (denominator ,(->jstring (number->string (denominator exact-number))))))))

(define (integer->jbigint number)
  (if (not (integer? number))
      (throw (make-error 'integer->jbigint "Integer expected: ~a" number))
      (j "new java.math.BigInteger(number);"
         `((number ,(->jstring (number->string number)))))))

(define-java-classes <java.util.concurrent.concurrent-hash-map>)

;; spec: (j "new java.util.concurrent.ConcurrentHashMap();")
(define-syntax create-concurrent-hash-map
  (syntax-rules ()
    ((_)           (java-new <java.util.concurrent.concurrent-hash-map>))
    ((_ capacity)  (java-new <java.util.concurrent.concurrent-hash-map> (->jint capacity)))))

(define-generic-java-method |put|)

(define (key-value->jmap m)
  (let ((result (create-concurrent-hash-map)))
    (for-each
     (match-lambda ((key value)
               ;; (j "map.put(key, value);" `((map ,result) (key ,(->jstring key)) (value ,(->jobject value))))
               (put result (->jstring key) (->jobject value))
               ))
     m)
    result))

(define (key-jvalue->jmap m)
  (let ((result (create-concurrent-hash-map)))
    (for-each
     (match-lambda ((key value)
               ;; (j "map.put(key, value);" `((map ,result) (key ,(->jstring key)) (value ,(->jobject value))))
               (put result (->jstring key) value)
               ))
     m)
    result))

(define (->jmap m)
  (let ((result (create-concurrent-hash-map)))
    (for-each
     (match-lambda
      
      ((key value)
       ;; (j "map.put(key, value);" `((map ,result) (key ,(->jstring key)) (value ,(->jobject value))))
       (put result (->jobject key) (->jobject value)))

      ((key . value)
       ;; (j "map.put(key, value);" `((map ,result) (key ,(->jstring key)) (value ,(->jobject value))))
       (put result (->jobject key) (->jobject value)))

      (#(key value)
       ;; (j "map.put(key, value);" `((map ,result) (key ,(->jstring key)) (value ,(->jobject value))))
       (put result (->jobject key) (->jobject value)))
      
      )
     m)
    result))

(define (jmap->alist c)
  (let ((it (j "c.entrySet().iterator();" `((c ,c)))))
    (map (lambda (e)
           (cons
            (->scm-object (j "e.getKey();" `((e ,e))))
            (->scm-object (j "e.getValue();" `((e ,e))))))
         (iterable->list it))))

;; Usage: (map-jarray (lambda (v) (->scm-object v)) (j "new int[]{1,2,3}")) === Returns ===> (1 2 3)
(define (map-jarray proc v)
  (let ((size (java-array-length v)))
    (let r ((i 0))
      (if (< i size)
          (cons (proc (java-array-ref v i)) (r (+ i 1)))
          '()))))

(define (make-atomic-boolean initial-value)
  (j "new java.util.concurrent.atomic.AtomicBoolean(initial);" `((initial ,(->jobject initial-value)))))

(define (compare-and-set-atomic-boolean! atomic-boolean-java-object expect update)
  (->scm-object (j "obj.compareAndSet(expect, update);" `((obj ,atomic-boolean-java-object)
                                                          (expect ,(->jobject expect))
                                                          (update ,(->jobject update))))))

(define (set-atomic-boolean! atomic-boolean-java-object new-value)
  (->scm-object (j "obj.set(update);" `((obj ,atomic-boolean-java-object)
                                        (update ,(->jobject new-value))))))

(define (get-atomic-boolean atomic-boolean-java-object)
  (->scm-object (j "obj.get();" `((obj ,atomic-boolean-java-object)))))

(define (make-atomic-long initial-value)
  (j "new java.util.concurrent.atomic.AtomicLong(initial);" `((initial ,(->jlong initial-value)))))

(define (compare-and-set-atomic-long! atomic-long-java-object expect update)
  (->scm-object (j "obj.compareAndSet(expect, update);" `((obj ,atomic-long-java-object)
                                                          (expect ,(->jlong expect))
                                                          (update ,(->jlong update))))))

(define (set-atomic-long! atomic-long-java-object new-value)
  (->scm-object (j "obj.set(update);" `((obj ,atomic-long-java-object)
                                        (update ,(->jlong new-value))))))

(define (get-atomic-long atomic-long-java-object)
  (->scm-object (j "obj.get();" `((obj ,atomic-long-java-object)))))

(define (inc-and-get-atomic-long! atomic-long-java-object)
  (->scm-object (j "obj.incrementAndGet();" `((obj ,atomic-long-java-object)))))

(define (add-and-get-atomic-long! atomic-long-java-object v)
  (->scm-object (j "obj.addAndGet(value);" `((obj ,atomic-long-java-object)
                                             (value ,(->jlong v))))))

(define (get-and-add-atomic-long! atomic-long-java-object v)
  (->scm-object (j "obj.getAndAdd(value);" `((obj ,atomic-long-java-object)
                                             (value ,(->jlong v))))))

(define (get-and-inc-atomic-long! atomic-long-java-object)
  (->scm-object (j "obj.getAndIncrement();" `((obj ,atomic-long-java-object)))))

(define (java-equals? obj1 obj2)
  (->scm-object (j "obj1.equals(obj2);" `((obj1 ,obj1)
                                          (obj2 ,obj2)))))

(define (safe-java-unwrap o) (if (java-null? o) '() (java-unwrap o)))

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
                           (for-each (lambda (v)

                                       (match v
                                              ( (vname vvalue) 
                                                (|set| tint (->jstring (if (string? vname) vname (symbol->string vname))) vvalue)
                                                )
                                              ( (vname)
                                                #t
                                                )
                                              )

                                       )
                                vars))))                                           
                 (let ((result (|eval| tint (->jstring str))))
                   ;; Clear memory.
                   (when (list? vars)
                     (for-each
                      (lambda (v)
                            (match v
                                        ( (vname vvalue) 
                                          (|set| tint (->jstring (if (string? vname) vname (symbol->string vname))) jnull)
                                          )
                                        ( (vname)
                                          (|set| tint (->jstring (if (string? vname) vname (symbol->string vname))) jnull)
                                          )
                                  )
                             ) vars))
                   result)
                 )))

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

