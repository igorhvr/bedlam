(define (locate channel message ignore term)
  (with-output-to-string
    (lambda ()
      (display (format "~a is in " term))
      (let loop ([o (channels-user-occupies (metaphone term))])
        (if (null? o) (display #\.)
            (begin
              (display (car o))
              (if (not (null? (cdr o)))
                  (display 
                   (if (not (null? (cddr o)))
                       ", " " and ")))
              (loop (cdr o))))))))
