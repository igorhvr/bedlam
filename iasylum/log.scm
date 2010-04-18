;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/iasylum))

(module iasylum/log
  (log-m log-o get-timestamp make-mark-logger log-debug log-info log-warn log-error log-fatal timestamped-log)

  ;;  (require-extension (lib iasylum/jcode))
  
  (define (log-m m)
    (send-email (mail-server) "igorhvr@localhost" "igorhvr" (system-email) (system-handle) m m))
  
  (define (log-o m)
    (j "System.out.println(m); System.out.flush();" `((m ,(->jstring m))))
    (void))
  
  (define (get-timestamp)
    (->string (j "new java.text.SimpleDateFormat(\"yyyy-MM-dd HH:mm:ss.SSS\").format(new java.util.Date());")))
  
  (define make-mark-logger (lambda (mark) (lambda m (timestamped-log mark m))))
  
  (define log-debug (make-mark-logger "DEBUG"))
  (define log-info (make-mark-logger "INFO"))
  (define log-warn (make-mark-logger "WARN"))
  (define log-error (make-mark-logger "ERROR"))
  (define log-fatal (make-mark-logger "FATAL"))

  (define (get-thread-info)
    (string-append "[thread=" (number->string (->number (j "Thread.currentThread().getId();"))) "]"))
  
  (define (timestamped-log mark m)
    (log-o (iasylum-write-string (list mark (get-thread-info) (get-timestamp) m)))))
