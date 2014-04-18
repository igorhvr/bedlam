(module iasylum/math
  (fact log* mod exact-floor)

  (define (exact-floor n)
    (let ((n (inexact->exact n)))
      (let ((up (numerator n)) (down (denominator n))) (/ (- up (mod up down)) down))))
  
  (define mod modulo)
  (define (fact n) (if (or (= 0 n) (= 1 n)) 1 (* n (fact (- n 1)))))
  (define* (log* arg1 (arg2 #f)) (if (not arg2) (log arg1) (let ((b arg1) (M arg2)) (/ (log M) (log b)))))
)  