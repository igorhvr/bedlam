(import hashtable)

(define breakpoints (make-hashtable eqv?))

(define (breakpoint-set? uid) 
  (hashtable/get breakpoints uid))

(define (set-uid-breakpoint! uid . metadata)
  (hashtable/put! breakpoints uid (if (null? metadata) #t
                                      (car metadata))))

(define (clear-breakpoint! uid)
  (hashtable/remove! breakpoints uid))

(define (breakpoint-list)
  (hashtable/map cons breakpoints))
                   
(define (set-file-breakpoint! file line col)
  (let ([bp (closest-ipoint file line col)])
    (and bp (let-values ([(line col uid) bp])
              (set-uid-breakpoint! uid (list file line col))))))