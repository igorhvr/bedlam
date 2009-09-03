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
