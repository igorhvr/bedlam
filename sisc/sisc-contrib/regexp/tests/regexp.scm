;; regexp.scm: testsuite for the SISC regexp binding module to Jakarta ORO
;;
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Date: December 15, 2001
;;
;; Copyright (C) 2001 Ovidiu Predescu
;; All rights reserved.
;;
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose and without fee is hereby granted, provided
;; that the above copyright notice appear in all copies and that both that
;; copyright notice and this permission notice appear in supporting
;; documentation.
;;
;; We disclaim all warranties with regard to this software, including all
;; implied warranties of merchantability and fitness, in no event shall
;; we be liable for any special, indirect or consequential damages or any
;; damages whatsoever resulting from loss of use, data or profits, whether in
;; an action of contract, negligence or other tortious action, arising out of
;; or in connection with the use or performance of this software.

;;;; assert.scm

(define failures 0)
(define passes 0)

(define-syntax assert
  (lambda (x)
    (syntax-case x (passes failures)
      ((_ op? e1 e2 . args)
	   (syntax
        ((lambda (a b)
           (if (op? a b)
               (set! passes (+ passes 1))
               (begin
                 (set! failures (+ failures 1))
                 (display (format "failed ~s = ~s, got ~s, expected ~s"
                                  (syntax-object->datum (syntax e1))
                                  (syntax-object->datum (syntax e2))
                                  e1 e2))
                 (if (> (length (syntax-object->datum (syntax args))) 0)
                     (begin
                       (display ": ")
                       (map display (syntax-object->datum (syntax args)))))
                 (newline))))
         e1 e2))))))
  
(define-syntax assertEqual
  (lambda (x)
    (syntax-case x ()
      ((_ e1 e2) (syntax (assert equal? e1 e2)))
      ((_ e1 e2 args) (syntax (assert equal? e1 e2 args))))))
  
(define-syntax assertNotEqual
  (lambda (x)
    (syntax-case x ()
      ((_ e1 e2)
       (syntax (assert (lambda (a b) (not (equal? a b))) e1 e2)))
      ((_ e1 e2 args)
       (syntax (assert (lambda (a b) (not (equal? a b))) e1 e2 args))))))
  
(define-syntax test
  (lambda (x)
    (syntax-case x (passes failures)
      ((_) 
       (syntax
        (begin
          (display (format "~s passes, ~s failures~%" passes failures))
          (set! passes 0)
          (set! failures 0))))
       ((_ e1 e2 ...)
        (syntax (begin e1 (test e2 ...)))))))

;;;; regexp.scm - test regexp functions

(load-module "sisc.contrib.regexp.Regexp$Index")

(test
 (define p1 "abcaaab")

 ;; Test if regexp-match accepts both strings and regexp objects
 (assertEqual (regexp-match "ab" p1) '("ab" "ab"))
 (assertEqual (regexp-match (regexp "ab") "abcdaaab") '("ab" "ab"))

 ;; Test if regexp-match-positions accepts both strings and regexp objects
 (assertEqual (regexp-match-positions "ab" "abcdaaab") '((0 . 2) (6 . 8)))
 (assertEqual (regexp-match-positions (regexp "ab") "abcdaaab") '((0 . 2) (6 . 8)))

 ;; Test if regexp-replace accepts both strings and regexp objects
 (assertEqual (regexp-replace "ab" p1 "xy") "xycaaab")
 (assertEqual (regexp-replace (regexp "ab") p1 "xy") "xycaaab")

 ;; Test if regexp-replace* accepts both strings and regexp objects
 (assertEqual (regexp-replace* "ab" p1 "xy") "xycaaxy")
 (assertEqual (regexp-replace* (regexp "ab") p1 "xy") "xycaaxy")

 ;; Test if regexp-split accepts both strings and regexp objects
 (assertEqual (regexp-split "/([ ,])/" "a b, c") #("a" " " "b" "," "" " " "c"))
 (assertEqual (regexp-split (regexp "/([ ,])/") "a b, c") #("a" " " "b" "," "" " " "c"))

 ;; Test if regexp-split/delimiter accepts both strings and regexp objects
 (assertEqual (regexp-split/delimiter "[ ,]+" "a b, c") #("a" "b" "c"))
 (assertEqual (regexp-split/delimiter (regexp "[ ,]+") "a b, c") #("a" "b" "c"))

 ;; Test Perl5 patterns with different options
 (assertEqual (regexp-match (regexp "a.*b" 'perl5) "abaac") '("ab"))
 ; case insensitive
 (assertEqual (regexp-match (regexp "a.*b" 'perl5 '(case-insensitive)) "ABaac") '("AB"))
 ; singleline: pattern accross lines
 (assertEqual (regexp-match (regexp "a.*b" 'perl5 '(case-insensitive singleline)) (format "acd~%bc")) (list (format "acd~%b")))
 ; multiline: same pattern and input as above; should fail
 (assertNotEqual (regexp-match (regexp "a.*b" 'perl5 '(case-insensitive multiline)) (format "acd~%bc")) (list (format "acd~%b")))

 ;; Test grouping
 (assertEqual (regexp-match "abc/(.*)" "abc/123") '("abc/123" "123"))
 (assertEqual (regexp-match-positions "abc/(.*)" "abc/123") '((0 . 7) (4 . 7)))

 (assertEqual (regexp-match "abc/([^/]*)/([^/]*)" "abc/123/def") '("abc/123/def" "123" "def"))
 )
