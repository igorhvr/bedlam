#! /usr/bin/env scheme-srfi-0

;; Simple test harness, for a subset of the modules in this directory.

(define test-scripts
  '("sexp-xml-test.scm"
    "xmlrpc-test.scm"))


;; Usage:
;;   (expect id expected body ...)
;;       evaluate BODY... and compare with EXPECTED
;;   (expect-failure id body ...)
;;       ensure that BODY... throws an error

(define test-cases
  (let ((nfails 0))
    (lambda (state)
      (cond ((eq? state 'begin)
             (set! nfails 0))
            ((eq? state 'increment)
             (set! nfails (+ nfails 1)))
            ((eq? state 'end)
             (if (= nfails 0)
                 (format #t "...all tests passed~%")
                 (format #t "...failures: ~a~%" nfails))
             (set! nfails 0))))))

(define-syntax expect
  (syntax-rules ()
    ((_ id expected body ...)
     (with/fc (lambda (m e)
                (format #t "Test ~a~%    produced error~a: ~a~%    expected ~s~%"
                          (quote id)
                          (if (error-location m)
                              (format #f " in ~a" (error-location m))
                              "")
                          (or (error-message m)
                              (error-message (error-parent-error m))
                              (error-message
                               (error-parent-error
                                (error-parent-error m)))
                              m)
                          expected)
                (test-cases 'increment))
       (lambda ()
         (let ((test ((lambda ()
                        body ...))))
           (if (not (equal? expected test))
               (begin (format #t "Test ~a~%    produced ~s~%    expected ~s~%"
                              (quote id) test expected)
                      (test-cases 'increment)))))))))
(define-syntax expect-failure
  (syntax-rules ()
    ((_ id body ...)
     (with/fc (lambda (m e)
                ;;failed -- OK
                #t)
        (lambda ()
          (let ((test ((lambda () body ...))))
            (format #t "Test ~a~%    produced ~s~%    expected ERROR~%"
                    (quote id) test)
            (test-cases 'increment)))))))

;; Run the list of test scripts
(for-each (lambda (script)
            (let ()
              (format #t "Running ~a...~%" script)
              (test-cases 'begin)
              (load script)
              (test-cases 'end)))
          test-scripts)
