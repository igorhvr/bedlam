;;; Copyright (c) 2005, 2006 LShift Ltd. <query@lshift.net>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(require-library 'sisc/libs/srfi/srfi-13) ; string library


(module lshift/error
    (error+
     debug-let
     debug-dump-exception)

  (import debugging)
  (import srfi-13)
  (import string-io)

  (define (error+ msg . args)
    (error (string-concatenate (cons msg
				     (map (lambda (arg)
					    (string-append "; "
							   (with-output-to-string
							     (lambda () (write arg)))))
					  args)))))

  (define-syntax debug-let
    (syntax-rules ()
      ((_ which-let ((binding init) ...) body ...)
       (which-let ((binding (let ((temp init))
			      (pretty-print `(debug-let which-let binding ,temp))
			      (newline)
			      temp))
		   ...)
	 body ...))))

  (define (debug-dump-exception m e)
    (define (d . xs) (for-each (lambda (x) (display x (current-error-port))) xs))
    (d "\n===========================================================================\n")
    (d "SCHEME-LEVEL ERROR\n\n")
    (for-each (lambda (pair) (d (car pair)": "(cdr pair)"\n"))
	      m)
    (d "\nSTACK TRACE\n\n")
    (d (with-output-to-string (lambda () (print-stack-trace e))))
    (d "\n===========================================================================\n")))
