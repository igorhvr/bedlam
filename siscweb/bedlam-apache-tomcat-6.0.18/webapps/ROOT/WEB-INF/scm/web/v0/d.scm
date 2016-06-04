(require-extension (lib siscweb/publish))
(require-extension (lib siscweb/request))
(require-extension (lib siscweb/response))
(require-extension (lib siscweb/html))

(extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/WEB-INF/classes/")

(define (setup-easy-debugging-at-large-performance-cost)
 (max-stack-trace-depth 32)
 (emit-annotations #t)
 (emit-debugging-symbols #t)
 (stack-trace-on-error #t))

(setup-easy-debugging-at-large-performance-cost)

(define (reload)
  (load "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/WEB-INF/scm/web/v0/d.scm"))

(require-extension (lib iasylum/javascript))

(define i18n.js (file->string "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/s/i18n.js"))
(define dic.js (file->string "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/s/dic.js"))

(js dic.js)
(js i18n.js)

(define get-message
  (lambda* (message (lang "pt"))
      (if (and (irregex-match '(* (or alphanumeric (" áàéèíìóòúùãẽĩõũçćñ¿?¡!%/$()_.,-+=/\\?|#$&*"))) lang)
               (irregex-match '(* (or alphanumeric (" áàéèíìóòúùãẽĩõũçćñ¿?¡!%/$()_.,-+=/\\?|#$&*"))) message))
          ;; TODO: pass parameters instead of string contact (broken).
          (->string (js (string-append "getMessage_engine(\"" message "\", \"" lang "\");")))
          (string-append lang "_" message "_untranslated"))))

(define (send-json json)
  (send-html/back
   '(("Content-Type" "application/json"))
   `(*VERBATIM* ,json)))

(define (input-port->string-truncated-at-x-bytes input x)
    (define i (if (binary-input-port? input)
                  (open-character-input-port input)
                  input))
    (define o (open-output-string))      
    (define mbuffer (make-string x))
    (define (loop)
      (let ((a (read-string mbuffer 0 (string-length mbuffer) i)))
        (if (eof-object? a) 'done 
            (begin
              (write-string mbuffer 0 a o)
              (loop)))))
    (loop)
    (let ((final-result (get-output-string o)))
      final-result))

(log-info 'd-startup-finished)
