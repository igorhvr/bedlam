(module streams ((stream-cons) (stream-cdr) ordered-stream-map stream-for-each)
  (define-syntax stream-cons
    (syntax-rules ()
      ((_ car cdr)
       (cons car (delay cdr)))))
  (define-syntax stream-cdr
    (syntax-rules ()
      ((_ kons)
       (force (cdr kons)))))
  (define (stream-for-each proc ls)
    (unless (null? ls) 
      (proc (car ls))
      (stream-for-each proc (stream-cdr ls))))
  (define (ordered-stream-map proc ls)
    (if (null? ls) '()
        (let ([res (proc (car ls))])
          (cons res (ordered-stream-map proc (stream-cdr ls)))))))


