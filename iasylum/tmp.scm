; Scratchpad / random examples, complete mess.

; Naive version.
;(define (flatten l)
;  (cond ((null? l) '())
;        ((list? l)
;         (append (flatten (car l)) (flatten (cdr l))))
;        (else (list l))
;  )
;)

; High performance one:
(define (flatten tree)
  (define result (list))
  ;; This leaves nothing pending on the stack, and doesn't build
  ;; intermediate results that it throws away.
  (define (flatten-element elt)
    (if (list? elt)
        (flatten-a-list elt)
        (set! result (cons elt result))))
  (define (flatten-a-list lst)
    (for-each flatten-element lst))

  (if (list? tree)
      (begin
        (flatten-a-list tree)
        (reverse! result))
      tree))

(define (is-zip? file-name)
  (let ((unzip-results (list-ref (r-split (string-append "unzip -z \"" file-name "\"")) 2)))
    (string=? unzip-results "")))
;         (let ((expr '(: "End-of-central-directory signature not found.")))
;           (let ((return (irregex-search  expr unzip-results)))
;             (d "\n(is-zip? \"" file-name "\") => \n Unzip-results=")
;             (w unzip-results)
;             (d " & will return " (not return) ".\n")
;             (not return))))))

(import file-manipulation)

(define (rglob dir)
  (define (do-rglob dir)
    (let ((d-contents (directory-list dir)) )
      (map (lambda (p)
           (let ((item (string-append dir "/" p)))
                  (if (file-is-directory? item) (do-rglob item) item)))
                     d-contents)))
  (flatten (do-rglob (string-append dir))))

(define (zip-split dir)
  (d "\n\n")
  (let ((files (rglob dir)))
    (for-each
     (lambda (f)
       (j "System.gc();")
       (let ((zip? (is-zip? f)))
        (if zip?
             (d "\n(zip \"" f "\")")
                  (d "\n(not-zip \"" f "\")"))))
     files)
    (d "\n\n")
    ))


(define (save-object-to object to)
  (import serial-io)
  (call-with-serial-output-file
   to
   (lambda (p)
     (serialize object p))))

(define (read-object from)
  (import serial-io)
  (call-with-serial-input-file
   from
   (lambda (p)
     (deserialize p))))

(define (base64-test tstring)
  (let ((base64-encoded-str
         (->scm-object
          (j
           "javax.xml.bind.DatatypeConverter.printBase64Binary(tstring.getBytes());"
           `((tstring ,(->jstring tstring)))))))
    (d/n "base64-encoded-str: " base64-encoded-str)
    (let ((tbytes (j (quote-convert "javax.xml.bind.DatatypeConverter.parseBase64Binary(data);")
                     `((data ,(->jstring base64-encoded-str))))))
      (d/n "bytes: " tbytes)
      (->scm-object (j "new String(tbytes);" `((tbytes ,tbytes)))))))

(define (base64-encode o) (j "javax.xml.bind.DatatypeConverter.printBase64Binary(data.toString().getBytes());" `((data ,(->jobject o)))))

(define (base64-decode o) (j "new String(javax.xml.bind.DatatypeConverter.parseBase64Binary(data.toString()));" `((data ,(->jobject o)))))

(define (url-encode o) (->scm-object (j "java.net.URLEncoder.encode(data.toString(),\"UTF-8\");" `((data ,(->jobject o))))))

(define (url-decode o) (->scm-object (j "java.net.URLDecoder.decode(data.toString(),\"UTF-8\");" `((data ,(->jobject o))))))

;; Example - how to get an sc-expanded version of the code in a given file.
;; (sc-expand (call-with-input-string (file->string "/tmp/example.scm") (lambda (input) (read input)) ))

;; match-let sample
;;  (match-let ((#(("param1" . pu)("param2" . pd))data-to-be-matched))  (d/n "pu " pu " pd: " pd))

;; Interesting call-with-values example.
(call-with-values (lambda () (parallel (lambda () (+ 3 4)) (lambda ()  (+ 5 7)))) string-append*)

(loop lp ((n <- in-range 1 5 1)) (d/n n) (lp))

;(apply parallel (map (lambda (fn) (lambda () (try-and-if-it-fails-object (#f) (smart-compile fn)))) (find-scm-in-directory "/base/bedlam/iasylum")))

;(map (lambda (fn) (try-and-if-it-fails-object (#f) (smart-compile fn))) (find-scm-in-directory "/base/bedlam/iasylum"))

; Fun match sample.
((lambda (p) (letrec ((fn (match-lambda ((lonely-element-in-a-list) lonely-element-in-a-list) ((first . rest) (string-append* first "," (fn rest)))))) (fn p))) '(1 2 3 4))

(letrec ((fn (match-lambda ((lonely-element-in-a-list)
                       lonely-element-in-a-list)
                      ((first . rest) (string-append* first "," (fn rest))))))
  (fn p))

(define (force-integer-to-digits number digits)
  (fmt #f (pad-char #\0  (pad/left digits (num number)))))

;(ssax:xml->sxml (open-input-string "<?xml?><cat a=\"b\"></cat>") '()) ===> (*top* (*pi* xml "") (cat (|@| (a "b"))))

(define sample-text "{ \"father\": { \"second-father\": { \"third-father\": \"someValueHere\"} } }")
(match ((sxpath "/father/second-father/third-father") (json->sxml sample-text)) (((_ d)) d))

;; I just *had* to have a y-combinator example here. ;-)
;; You should probably use (fold (cute string-append <> "," <>) "" list) instead...
(define email-list->destination-string 
  (y-combinator
   (lambda (recur)
     (match-lambda
      (() "")
      ((e . ()) e)
      ((e . rest) (string-append e " , " (recur rest)))
      (anything (error "Invalid parameter:" anything))))))

;Generates lines to compile each scheme file....

(each-for (filter (lambda (v) (irregex-search "scm$" v)) (rglob "/home/igorhvr/idm/bedlam/iasylum")) (lambda (v) (d/n (string-append* "/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location \"/base/bedlam/\") (load (string-append iasylum-bedlam-location \"iasylum/init.scm\")) (d/n \"Will compile\" \"" v "\")(with/fc (lambda p (display p) (j \"System.exit(0);\")) (lambda () (smart-compile \"" v "\")))(j \"System.exit(0);\")'"))))

(define (jvm-max-memory)
 ->number (j "Runtime.getRuntime().maxMemory();"))

(define try-with-exponential-backoff
  (lambda* ((action: action #f) (action-description: action-description (iasylum-write-string action))
       (initial-interval-millis: initial-interval-millis 50)
       (max-elapsed-time-millis: max-elapsed-time-millis 2147483647) ;; Aprox 24 days
       (max-interval-millis: max-interval-millis 30000)
       (log-error: log-error log-error))
      (when (not action)
          (d/n "Mandatory parameter action not provided. Sample usage: " "(try-with-exponential-backoff 'action: (lambda () (/ 2 3) (/ 2 0)) 'action-description: \"Let's try to divide by zero.\" 'initial-interval-millis: 50 'max-interval-millis: 200 'max-elapsed-time-millis: 1000 'log-error: d/n)")
          (error "No action provided for trying with exponential backoff."))
      (let ((auto-retry (j "new com.google.api.client.util.ExponentialBackOff.Builder().
                            setInitialIntervalMillis(iim).setMaxElapsedTimeMillis(metm).
                            setMaxIntervalMillis(mim).setMultiplier(1.5).setRandomizationFactor(0.5).build();"
                           `((iim ,(->jint initial-interval-millis))
                             (metm ,(->jint max-elapsed-time-millis))
                             (mim ,(->jint max-interval-millis))))))
        (let try-again ()
          (with-failure-continuation
           (lambda (error error-continuation)
             (let ((ms-to-wait (->scm-object (j "backoff.getCurrentIntervalMillis();" `((backoff ,auto-retry)))))
                   (ms-elapsed (->scm-object (j "backoff.getElapsedTimeMillis();" `((backoff ,auto-retry)))))
                   (ms-max (->scm-object (j "backoff.getMaxElapsedTimeMillis();" `((backoff ,auto-retry))))))
               (log-error (format "Error when trying to perform the following action: ~a . I'll try again after ~a ms. Elapsed: ~a ms, Max: ~a ms."
                                  action-description ms-to-wait ms-elapsed ms-max) error)
               (sleep ms-to-wait)
               (let ((next-backoff (->number (j "backoff.nextBackOffMillis();" `((backoff ,auto-retry))))))
                 (if (= -1 next-backoff)
                     (throw
                      (make-nested-error
                       (make-error
                        "max-elapsed-time-millis reached while performing retries with exponential backoff strategy. Giving up.")
                       error error-continuation))
                     (try-again)))))
           (lambda ()
             (action)))))))
