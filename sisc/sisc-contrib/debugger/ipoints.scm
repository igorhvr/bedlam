(import hashtable)

(define ipoint-files (make-hashtable equal?))
(define ipoints (make-hashtable eq?))

(define (register-ipoint file line col uid)
  (hashtable/put! ipoint-files file 
                  (cons (list line col uid)
                        (hashtable/get ipoint-files file '())))
  (hashtable/put! ipoints uid (list file line col)))

(define (locate-ipoint uid)
  (apply values (hashtable/get! ipoints uid '(#f #f #f))))

(define (closest-ipoint file line col)
  (let loop ([points (hashtable/get ipoint-files file '())]
             [best-line-diff #x7ffffff] [best-line #f]
             [best-col-diff #x7ffffff] [best-col #f]
             [best-uid #f])
    (if (null? points)
        (and best-uid (values best-line best-col best-uid))
        (let ([cl (caar points)]
              [cc (cadar points)])
          (cond [(or (< cl line) (> (- cl line) best-line-diff)
                     (< cc col) (> (- cc col) best-col-diff))
                 (loop (cdr points) best-line-diff best-line
                       best-col-diff best-col best-uid)]
                [else (loop (cdr points) (- cl line) cl (- cc col) cc 
                            (caddar points))])))))
