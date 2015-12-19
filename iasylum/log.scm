;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/iasylum))
(require-extension (lib iasylum/work-queue))

(module iasylum/log
  (
   make-logger
   make-human-logger
   make-empty-logger
   get-global-logger-to-this-thread
   set-global-logger-to-this-thread!
   with-logger

   get-thread-info
   log-o
   get-timestamp
   make-mark-logger
   timestamped-log
   log-trace
   log-debug
   log-info
   log-warn
   log-error
   log-fatal)

  (define o-work-queue (make-queue))
  
  (define log-o)

  (define-generic-java-field-accessors |out|)
  (define-java-class |java.lang.System|)
  (define-generic-java-method |println|)
  
  (define (o-worker m)
    ;spec: (j "System.out.println(m); System.out.flush();" `((m ,(->jstring m))))
    (|println| (|out| (java-null |java.lang.System|)) (->jstring m))
    (void))

  (define-java-class |java.text.SimpleDateFormat|)
  (define-java-class |java.util.Date|)
  (define-generic-java-method |format|)
  
  (define *ts-format* (->jstring "yyyy-MM-dd HH:mm:ss,SSS")  )
  (define (get-timestamp)
    (->string
     
     ; spec: ;(j "new java.text.SimpleDateFormat(\"yyyy-MM-dd HH:mm:ss,SSS\").format(new java.util.Date());")
     (|format| (java-new |java.text.SimpleDateFormat| *ts-format* ) (java-new |java.util.Date|))
     
     ))
  
  (define make-mark-logger (lambda (mark) (lambda (thread-info timestamp m) (timestamped-log mark thread-info timestamp m))))

  (define-generic-java-method |getName|)
  (define-generic-java-method |currentThread|)
  (define-java-class |java.lang.Thread|)
  
  (define (get-thread-info)
    (->string
     
     ;spec: (j "Thread.currentThread().getName();")
     (|getName| (|currentThread| (java-null |java.lang.Thread|)))
     
     ))
  
  (define (timestamped-log mark thread-info timestamp m)
    (log-o (iasylum-write-string (list mark thread-info timestamp m))))

  (let-syntax ((define-log-fn (syntax-rules ()
                                [(_ name level-symbol)
                                 (define name
                                   (and-let* ((work-queue (make-queue))
                                              (p-fn (work-queue 'put-scm-lambda))
                                              (worker (match-lambda* ([(thread-info timestamp params)]
                                                        (apply (get-global-logger-to-this-thread)
                                                               `(,thread-info ,timestamp ,level-symbol ,params)))))
                                              ((start-worker worker work-queue 'continue-forever: #t 'log-trace-execution: (make-parameter* #f))))
                                     (lambda params
                                       (p-fn (list (get-thread-info) (get-timestamp) params)))))])))
    
    (begin
      (define-log-fn log-trace 'trace)
      (define-log-fn log-debug 'debug)
      (define-log-fn log-info 'info)
      (define-log-fn log-warn 'warn)
      (define-log-fn log-error 'error)
      (define-log-fn log-fatal 'fatal)))

  (define (get-global-logger-to-this-thread)
    (let ((global-logger (j "iu.M.d.get(\"logger-global_48729\");")))
      (if (equal? (j "null")
                  global-logger)
          (begin
            (set-default-global-logger!)
            (get-global-logger-to-this-thread))
          (->scm-object (j "globall.get();"
                           `((globall ,global-logger)))))))

  (define (set-global-logger-to-this-thread! logger)
    (j "iu.M.d.get(\"logger-global_48729\").set(newlogger);"
       `((newlogger ,(->jobject logger)))))

  ;;
  ;; It is useful when you will use the same information several times,
  ;; e.g. transaction-id.
  ;;
  ;; Example:
  ;; (make-logger <module-name> <transaction-id> <ip> <any-important-information> ...)
  ;; => logger
  ;;
  ;; using logger:
  ;; (logger 'trace "message1" "message2")
  ;; => ("TRACE" "[thread=9]" "2014-05-20 05:11:06.506"
  ;;             (<module-name> <transaction-id> <ip>
  ;;              <any-important-information> ... "message1" "message2"))
  ;;
  (define-syntax make-logger
    (syntax-rules ()
      ((_ <custom-info> ...)
       (let* ((log-trace (make-mark-logger "TRACE"))
              (log-debug (make-mark-logger "DEBUG"))
              (log-info  (make-mark-logger "INFO"))
              (log-warn  (make-mark-logger "WARN"))
              (log-error (make-mark-logger "ERROR"))
              (log-fatal (make-mark-logger "FATAL"))
              (log-hash (alist->hashtable `((trace . ,log-trace)
                                            (debug . ,log-debug)
                                            (info  . ,log-info)
                                            (warn  . ,log-warn)
                                            (error . ,log-error)
                                            (fatal . ,log-fatal)))))
         (lambda (thread-info timestamp level . message)
           (apply (cut (hashtable/get log-hash level log-error)
                       thread-info
                       timestamp
                       <custom-info> ... <...>) message))))))

  (define-syntax make-human-logger
    (syntax-rules ()
      ((_ min-level)
       (let ((level-hash (alist->hashtable `((trace . 0)
                                             (debug . 1)
                                             (info  . 2)
                                             (warn  . 3)
                                             (error . 4)
                                             (fatal . 5)))))
         (lambda (level . message)
           (if (>= (hashtable/get level-hash level 0)
                   (hashtable/get level-hash min-level 0))
               (apply d (add-between-elements "\n" (append (list (string-append* "______ "
                                                                                 (string-upcase (symbol->string level))
                                                                                 " ________________________"
                                                                                 " (" (get-thread-info) ") "
                                                                                 (get-timestamp)
                                                                                 "\n"))
                                                           message (list "\n"))))))))))

  (define (make-empty-logger)
    (lambda (level . message) #t))

  (define-syntax with-logger
    (syntax-rules ()
      ((_ logger body ...)
       (let ((old-logger (get-global-logger-to-this-thread)))
         (dynamic-wind (lambda () (set-global-logger-to-this-thread! logger))
                       (lambda () body ...)
                       (lambda () (set-global-logger-to-this-thread! old-logger)))))))

  (define (set-default-global-logger!)
    (j "globalLogger = new ThreadLocal() {
            protected Object initialValue() {
                return defaultlogger;
            }
        };

        iu.M.d.putIfAbsent(\"logger-global_48729\", globalLogger);"
       `((defaultlogger ,(->jobject (make-logger))))))

  (set-default-global-logger!)

  
  (set! log-o
        (let ((p-fn (o-work-queue 'put-scm-lambda)))
          (lambda (m)
           (p-fn m)
           (void))))
  
  (start-worker o-worker o-work-queue 'continue-forever: #t 'log-trace-execution: (make-parameter* #f))

  (start-worker (lambda (m) (apply log-trace m))  put-log-trace 'continue-forever: #t 'log-trace-execution: (make-parameter* #f))
 )
  
