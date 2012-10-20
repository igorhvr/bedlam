(define (yow channel message ignore term)
    (or (and (or (not ignore) (equal? ignore ""))
           (or (not term) (<= (string-length term) 1))
           (fetch-pinhead-quote!))
      'continue))

(define (fetch-pinhead-quote!)
  (if (null? zippy-ptr)
      (begin (rerandomize-zippy!)
             (fetch-pinhead-quote!))
      (let ([quote (car zippy-ptr)])
        (set! zippy-ptr (cdr zippy-ptr))
        quote)))
             
(define (read-to-null in)
  (with-output-to-string 
    (lambda ()
       (let loop ([c (read-char in)])
         (unless (or (eof-object? c) (eqv? c #\nul))
           (display c)
           (loop (read-char in)))))))

(define zippy)
(define zippy-ptr)

(define (insert-randomly! elem ls)
  (import srfi-1)
  (if (null? ls) 
      (list elem)
      (let ([base (drop ls (random-integer (length ls)))])
       (set-cdr! base (cons (car base) (cdr base)))
       (set-car! base elem)
       ls)))

(define (rerandomize-zippy!)
  (set! zippy-ptr
    (let loop ([ls zippy] [acc '()])
      (if (null? ls)
          acc
         (loop (cdr ls) (insert-randomly! (car ls) acc))))))

(define (init-zippy)
  (set! zippy
        (let ([inf (open-input-file "plugins/yow.lines")])
          (read-to-null inf)
          (let loop ([line (read-to-null inf)] [acc '()])
            (if (equal? "" line) 
                acc
                (loop (read-to-null inf) (insert-randomly! (trim line)
acc))))))
  (set! zippy-ptr zippy))

  