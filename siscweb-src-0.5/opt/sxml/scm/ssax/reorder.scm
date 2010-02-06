;; Reorders top-level s-expressions in a file to obey
;; module rules of SISC and other scheme environments:
;; pure definitions go first, all expressions go after
;; Arguments: input and output file names
;; Test case (save to a file, perhaps pasting to
;; "cut -c4- >test.scm")
;; (define (f x) x)
;; (define g (f 3))
;; (define-macro (m 1) 1)
;; (set! g (f 4))
;; g
;; (define (u x) 0)

(define (module-order iname oname)
  (let ((output (lambda (x o)
                  (write x o) (newline o))))
    (call-with-output-file oname
      (lambda (o)
        (call-with-input-file iname
          (lambda (i)
            (let loop ((expr-list '()))
              (let ((x (read i)))
                (if (eof-object? x)
                    (map
                     (lambda (expr) (output expr o))
                     (reverse expr-list))
                    (let ((stays? #t) (naked-def? #f))
                      (if (not (list? x))
                          (set! stays? #f)
                          (let ((first (car x)))
                            (if (eqv? first 'define)
                                (if
                                 (not (list? (cadr
                                              x)))
                                 (begin
                                   (set! stays? #f)
                                   (set! naked-def?
                                         #t)))
                                (set! stays? (memv
                                              first '(define-syntax define-macro defmacro))))))
                      (if naked-def?
                          (output
                           `(define ,(cadr x)) o))
                      (if stays? (output x o))
                      (loop
                       (if stays?
                           expr-list
                           (cons
                            (if naked-def?
                                `(set! ,@(cdr x))
                                x)
                            expr-list))))))))))))
  #t)

