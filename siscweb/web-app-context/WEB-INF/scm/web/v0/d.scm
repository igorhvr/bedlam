(require-extension (lib siscweb/publish))
(require-extension (lib siscweb/request))
(require-extension (lib siscweb/response))
(require-extension (lib siscweb/html))
(require-extension (lib siscweb/xhtml))

(extend-classpath "/base/bedlam/siscweb/web-app-context/WEB-INF/classes/")

(define (setup-easy-debugging-at-large-performance-cost)
 (max-stack-trace-depth 32)
 (emit-annotations #t)
 (emit-debugging-symbols #t)
 (stack-trace-on-error #t))

(setup-easy-debugging-at-large-performance-cost)

(define (reload)
  (load "/base/bedlam/siscweb/web-app-context/WEB-INF/scm/web/v0/d.scm"))

(require-extension (lib iasylum/javascript))

(define i18n.js (file->string "/base/bedlam/siscweb/web-app-context/s/i18n.js"))
(define dic.js (file->string "/base/bedlam/siscweb/web-app-context/s/dic.js"))

(define get-message
  (lambda* (message (lang "pt"))
           (js dic.js)
           (js i18n.js)
           (->string (js "getMessage_engine(messagei18n, langi18n);" `((messagei18n ,(->jstring message)) (langi18n ,(->jstring lang)))))))

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

(publish/wildcard
  "/hello-world/*"
  (lambda (request)
    (set! request #f)
    (send-html/back
    `(html
      (head (title "Hello, world! in two languages"))
      (body (p ,(get-message "hello" "pt")) (p ,(get-message "hello" "en")))))))

(publish/wildcard
  "/nothing/*"
  (lambda (request)
    (log 'HERE)
    (send-html/back
    `(html
      (head (title "Hello, world! in two languages"))
      (body (p ,(get-message "hello" "pt")) (p ,(get-message "hello" "en")))))))

(publish/wildcard
  "/fail/*"
  (lambda (request)
    (fksflsakfja-FAIL-fsdlkfjdsalkf)))


 (define (hello-world request)
    (let loop ()
      (for-each
       (lambda (message)
         (send-xhtml/suspend
          (make-hello-world-page message)))
       messages)
      (loop)))

(publish
 "/text-accumulator"
  (lambda (request)
    (let ((whatever (make-parameter* "")))
      (let loop ()      
        (send-html/suspend
         (lambda (k-url)
           (let ((datafield (extract-single-binding "datafield" (get-bindings request))))
             (whatever (string-append* (whatever) datafield))
             `(html (head (title ,"Text accumulator"))
                    (body
                     (h3 "Text Accumulator")
                     (p ,(whatever))
                     
                     (form (@ (action ,k-url))
                           (input (@ (type "text") (name "datafield")))
                           (input (@ (type "submit") (name "ACCUMULATE!")))))))))
        
        (loop)))))

(publish
 "/three-steps-example"
  (lambda (request)
    (let ((whatever (make-parameter* "")))
      (send-html/suspend
         (lambda (k-url)
           `(html (head (title ,"STEP 1"))
                    (body
                     (h3 "First step")
                     
                     (form (@ (action ,k-url))
                           (input (@ (type "text") (name "datafield")))
                           (input (@ (type "submit") (value "Go to second"))))))))

      (send-html/suspend
         (lambda (k-url)
           `(html (head (title ,"STEP 2"))
                    (body
                     (h3 "Second step " ,(extract-single-binding "datafield" (get-bindings request)))
                     
                     (form (@ (action ,k-url))
                           (input (@ (type "submit") (value "GO TO FINAL PLACE."))))))))

      (send-html/finish
       `(html (head (title ,"STEP 3 - FINISHED! Thanks!"))
              (body
               (h3 "END.")))))))

 (define web/d/n
   (lambda p
     (let ((result (string-append* (map display-string (flatten (list "\n" p "\n\n"))))))
       (send-html/forward
        (lambda (k-url)
          `(html (head (title ,"web-d/n"))
                 (body
                  (h3 ,result)
                  
                  (form (@ (action ,k-url))
                        (input (@ (type "submit") (value "Next.")))))))))))

 (define web/read-line
   (lambda ()
     (extract-single-binding "datainput" (get-bindings 
                                          (send-html/forward
                                           (lambda (k-url)
                                             `(html (head (title ,"web-readline"))
                                                    (body
                                                     (form (@ (action ,k-url))
                                                           (input (@ (type "text") (name "datainput")))
                                                           (input (@ (type "submit") (value "Next."))))))))))))

(publish
 "/web/d/n-test"
   (lambda (request)
     (web/d/n "first")
     (web/d/n "now lets grab a value")
     (let ((xp (web/read-line)))
       (web/d/n "lets see")
       (web/d/n xp)
       (web/d/n "end."))))
 

(log-info 'd-startup-finished)
