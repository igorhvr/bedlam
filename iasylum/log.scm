;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/iasylum))
(require-extension (lib iasylum/work-queue))

(module iasylum/log
  (
   ; standard logs:
   log-trace
   log-debug
   log-info
   log-warn
   log-error
   log-fatal

   ; extra:
   log-and-return
   measure-time-interval-nano
   log-time
   append-to-loggers
   )

  (define-java-class |java.lang.System|)
  (define-generic-java-method |nanoTime|)
  (define-generic-java-method |trace|)
  (define-generic-java-method |debug|)
  (define-generic-java-method |info|)
  (define-generic-java-method |warn|)
  (define-generic-java-method |error|)
  (define-generic-java-method |fatal|)

  (define generic-log
    (let ((jlogger (j "org.apache.logging.log4j.LogManager.getLogger(\"app\");")))
      (lambda (level . messages)
        (let ((method (cond [(eq? level 'trace) |trace|]
                            [(eq? level 'debug) |debug|]
                            [(eq? level 'info)  |info|]
                            [(eq? level 'warn)  |warn|]
                            [(eq? level 'error) |error|]
                            [(eq? level 'fatal) |fatal|]
                            [else (throw (make-error "Invalid level: ~a" level))])))
          (method jlogger (->jstring (apply add-between (append (list " ")
                                                                messages))))))))

  (define log-trace (cut generic-log 'trace <...>))
  (define log-debug (cut generic-log 'debug <...>))
  (define log-info (cut generic-log 'info <...>))
  (define log-warn (cut generic-log 'warn <...>))
  (define log-error (cut generic-log 'error <...>))
  (define log-fatal (cut generic-log 'fatal <...>))

  ;;
  ;; Use like this: (append-to-loggers (log-trace log-debug log-info log-warn log-error log-fatal)
  ;;                                   ("attachment1" "attachment2" ...)
  ;;                                   body)
  ;;
  (define-syntax append-to-loggers
    (syntax-rules ()
      ((_ (log-trace log-debug log-info log-warn log-error log-fatal)
          (attachments ...)
          body ...)
       (let ((log-trace (cut log-trace attachments ... <...>))
             (log-debug (cut log-debug attachments ... <...>))
             (log-info (cut log-info attachments ... <...>))
             (log-warn (cut log-warn attachments ... <...>))
             (log-error (cut log-error attachments ... <...>))
             (log-fatal (cut log-fatal attachments ... <...>)))
         body ...))))

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
)
  
