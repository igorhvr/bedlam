(require-extension (lib iasylum/packrat))
(require-extension (srfi 69))
(require-extension (srfi 23))
(module iasylum/json
  (json-read json-write scheme->json json->scheme)
  
  (include "json/json-code.scm")
  
  (define (scheme->json structure)
    (call-with-output-string
     (lambda (output-port)
       (json-write
        structure
        output-port))))

  (define (decimal-to-fractions-inside-string s)  
    (irregex-replace/all '(seq
                           (submatch (? "-"))
                           (submatch (+ digit))
                           "."
                           (submatch (+ digit)))
                         s
                         (lambda (m)
                           (let ((sign (irregex-match-substring m 1))
                                 (beforedot (irregex-match-substring m 2))
                                 (afterdot (irregex-match-substring m 3)))
                             (number->string
                              (* (if (string=? "-" sign) -1 1)
                                 (+ (string->number beforedot)
                                    (/ (string->number afterdot) (expt 10 (string-length afterdot))))))))))
  
  (define json->scheme
    (lambda* (string (force-exact #f))
             (call-with-input-string
                 (if force-exact
                     (decimal-to-fractions-inside-string string)
                     string)
               (lambda (input-port)
                 (json-read input-port)))))

)