(require-extension (lib iasylum/irregex))

(module iasylum/math
  (fact log* mod exact-floor decimal-to-fractions-inside-string)

  (define (exact-floor n)
    (let ((n (inexact->exact n)))
      (let ((up (numerator n)) (down (denominator n))) (/ (- up (mod up down)) down))))
  
  (define mod modulo)
  (define (fact n) (if (or (= 0 n) (= 1 n)) 1 (* n (fact (- n 1)))))
  (define* (log* arg1 (arg2 #f)) (if (not arg2) (log arg1) (let ((b arg1) (M arg2)) (/ (log M) (log b)))))

  (define (decimal-to-fractions-inside-string s)
    (irregex-replace/all '(seq
                           (submatch (? "-"))
                           (submatch (* digit))
                           "."
                           (submatch (+ digit)))
                         s
                         (lambda (m)
                           (let ((sign (irregex-match-substring m 1))
                                 (beforedot (irregex-match-substring m 2))
                                 (afterdot (irregex-match-substring m 3)))
                             (number->string
                              (* (if (string=? "-" sign) -1 1)
                                 (+ (if (string=? "" beforedot) 0 (string->number beforedot))
                                    (/ (string->number afterdot) (expt 10 (string-length afterdot))))))))))
)  