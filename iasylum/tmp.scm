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
