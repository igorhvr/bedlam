(define generate-error-message (lambda () (/ 1 0))) ; TODO Cleanup.

(define random)
(define sample-random)
(define random-maker)

;;; Start - RANDOM UTILITIES ;;;

; A call to the random-maker procedure presented here yields a
; dynamically constructed procedure that acts as a random-number
; generator. When the dynamically constructed procedure is invoked
; with no arguments, it returns a pseudo-random real value evenly
; distributed in the range [0.0, 1.0); when it is invoked with
; one argument (which should be a positive integer n), it returns a
; pseudo-random integer value evenly distributed in the range [0, n);
; when it is invoked with two arguments, the first of which should be a
; positive integer and the second the symbol reset, it changes the seed
; of the random-number generator to the value of the first argument.
;
; The generator employs the linear-congruential method, and
; specifically uses a choice of multiplier that was proposed as a
; standard by Stephen K. Park et al. in ``Technical correspondence,''
; Communications of the ACM 36 (1993), number 7, 108--110.
;
;
(set! random-maker
      (let* ((multiplier 48271)
             (modulus 2147483647)
             (apply-congruence
              (lambda (current-seed)
                (let ((candidate (modulo (* current-seed multiplier)
                                         modulus)))
                  (if (zero? candidate)
                      modulus candidate))))
             (coerce
              (lambda (proposed-seed)
                (if (integer? proposed-seed)
                    (- modulus (modulo proposed-seed modulus))
                    generate-error-message)))) ; The user should give an integer seed. Otherwise screw him.
        (lambda (initial-seed)
          (let ((seed (coerce initial-seed)))
            (lambda args
              (cond ((null? args)
                     (set! seed (apply-congruence seed)) (/ (- modulus
                                                               seed) modulus))
                    ((null? (cdr args))
                     (let* ((proposed-top
                             (ceiling (abs (car args))))
                            (exact-top
                             (if (inexact? proposed-top)
                                 (inexact->exact proposed-top)
                                 proposed-top))
                            (top
                             (if (zero? exact-top)
                                 1 exact-top)))
                       (set! seed (apply-congruence seed)) (inexact->exact
                                                            (+ 0.0 (floor (* top (/ seed modulus)))) )))
                    ((eq? (cadr args) 'reset)
                     (set! seed (coerce (car args))))
                    (else
                     (display "random: unrecognized message")
                     (newline))))))))


(set! sample-random
 (random-maker 19781116))  ;; another arbitrarily chosen birthday
;
; The random procedure added at the end shows how to call
; random-maker to get a random-number generator with a specific
; seed. The random-number generator itself is invoked as described
; above, by such calls as (random), to get a real number between 0
; and 1, and (random n), to get an integer in the range from 0 to
; n - 1.  The location of the binding of seed -- inside the body
; of random-maker, but outside the lambda-expression that denotes
; the dynamically allocated procedure -- ensures that the storage
; location containing the seed will be different for each invocation of
; random-maker (so that every generator that is constructed will have
; an independently settable seed), yet inaccessible except through
; invocations to the dynamically allocated procedure itself. In
; effect, random-number generators in this implementation constitute
; an abstract data type with the constructor random-maker and exactly
; three operations, corresponding to the three possible arities of
; a call to the generator.
;
; When calling this procedure, the programmer must supply an initial
; value for the seed. This should be an integer (if it is not, an
; arbitrary default seed is silently substituted). The value supplied
; is forced into the range (0, modulus], since it is an invariant of
; the procedure that the seed must always be in this range.
;
; To obtain an initial seed that is likely to be different each time a
; new generator is constructed, use some combination of the program's
; running time and the wall-clock time. (Most Scheme implementations
; provide procedures that return one or both of these quantities. For
; instance, in SCM, the call
;
;
; (random-maker (+ (* 100000 (get-internal-run-time)) (current-time)))
;
; yields a generator with an effectively random seed.)
;
;
; --------------------------------------------------------------------------------
;
; This document is available on the World Wide Web as
;
;
; http://www.math.grin.edu/~stone/events/scheme-workshop/random.html
;
;
; --------------------------------------------------------------------------------
; created July 10, 1995 last revised June 21, 1996
;
; John David Stone (stone@math.grin.edu)



(set! random
     (let ((number-from-time
; SCSH            (call-with-values  (lambda () (time+ticks))                              (lambda (time ticks) (+ (* 1000000 time) ticks)))
            (->number (j "new java.util.Date().getTime();"))
            ))
       (let ((random-function (random-maker number-from-time)))
         (lambda* ((max-number 1000000000000))
           (random-function (+ max-number 0))))))

;;; End  - RANDOM UTILITIES ;;;