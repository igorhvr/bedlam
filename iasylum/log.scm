;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/iasylum))
(require-extension (lib iasylum/work-queue))

(module iasylum/log
  (
   ; logger flavors
   make-logger
   make-empty-logger

   ; under the hood
   get-global-logger-to-this-thread
   set-global-logger-to-this-thread!

   ; high level:
   with-logger
   log-and-return

   ; extra:
   measure-time-interval-nano

   ; facilities:
   log-time
   
   log-trace
   log-debug
   log-info
   log-warn
   log-error
   log-fatal)

  (define-java-class |java.lang.System|)

  (define-generic-java-method |nanoTime|)

  (let-syntax ((define-log-fn (syntax-rules ()
                                [(_ name level-symbol)
                                 (define name
                                   (lambda messages
                                     (apply (get-global-logger-to-this-thread) (cons level-symbol messages))))])))
    (begin
      (define-log-fn log-trace 'trace)
      (define-log-fn log-debug 'debug)
      (define-log-fn log-info 'info)
      (define-log-fn log-warn 'warn)
      (define-log-fn log-error 'error)
      (define-log-fn log-fatal 'fatal)))

  (define (get-global-logger-to-this-thread)
    (let ((global-logger (j "iu.M.d.get(\"logger-global_48729\");")))
      (if (equal? (java-null) global-logger)
          (make-display-logger)
          (->scm-object (j "globalthread.get();"
                           `((globalthread ,global-logger)))))))

  (define (set-global-logger-to-this-thread! logger)
    (j "iu.M.d.get(\"logger-global_48729\").set(newlogger);"
       `((newlogger ,(->jobject logger)))))

  (define make-logger
    (lambda extra-params
      (let ((jlogger (j "org.apache.logging.log4j.LogManager.getLogger(\"app\");")))
        (lambda (level . messages)
          (assert (or (eq? level 'trace)
                      (eq? level 'debug)
                      (eq? level 'info)
                      (eq? level 'warn)
                      (eq? level 'error)
                      (eq? level 'fatal)))
          (j (string-append* "logger." level "(message);")
             `((logger ,jlogger)
               (message ,(->jstring (apply add-between (append (list " ")
                                                               extra-params
                                                               messages))))))))))

  (define (make-empty-logger)
    (lambda (level . message) #t))

  (define-syntax with-logger
    (syntax-rules ()
      ((_ logger body ...)
       (let ((old-logger (get-global-logger-to-this-thread)))
         (dynamic-wind (lambda () (set-global-logger-to-this-thread! logger))
                       (lambda () body ...)
                       (lambda () (set-global-logger-to-this-thread! old-logger)))))))

  (define-syntax log-and-return
    (syntax-rules ()
      ((_ log-fn description body ...)
       (let ((result (begin body ...)))
         (log-fn description result)
         result))))

  ;;
  ;; Return a pair with the result and the time elapsed.
  ;;  
  (define (measure-time-interval-nano thunk)
    (let ((start-time (->number (|nanoTime| (java-null |java.lang.System|)))))
      (let ((result (thunk)))
        (let ((time-elapsed-nano (- (->number (|nanoTime| (java-null |java.lang.System|))) start-time)))
          (cons result time-elapsed-nano)))))

  ;;
  ;; Use like this:
  ;; (log-time ("debug info" log-debug) body ...)
  ;;
  ;; or:
  ;; (log-time ("debug info" log-debug log-warn 1 ms) body ...)
  ;;
  (define-syntax log-time
    (syntax-rules (ms)
      ((_ (debug-info log-fn log-slow-fn slow-ms ms) body ...)
       (let* ((result (measure-time-interval-nano (lambda () body ...)))
              (ms-elapsed (/ (cdr result) 1000000))
              (slow (> ms-elapsed slow-ms)))
         ((if (not slow)
              log-fn
              log-slow-fn)
          (|@srfi-28::format| "[~a]~aTook ~0,3F ms" debug-info (if slow " [TOO SLOW!] " " ") ms-elapsed))
         (car result)))
      ((_ (debug-info log-fn) body ...)
       (log-time (debug-info log-fn log-warn 1000 ms) body ...))))

  (j "globalLogger = new ThreadLocal() {
            protected Object initialValue() {
                return defaultlogger;
            }
        };

        iu.M.d.putIfAbsent(\"logger-global_48729\", globalLogger);"
     `((defaultlogger ,(->jobject (make-logger)))))
)
  
