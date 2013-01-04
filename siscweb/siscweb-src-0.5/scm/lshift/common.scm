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


(require-library 'sisc/libs/srfi/srfi-1) ; list library
(require-library 'sisc/libs/srfi/srfi-13) ; string library
(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures
(require-library 'sisc/libs/srfi/srfi-26) ; notation for specializing parameters without currying
(require-library 'sisc/libs/srfi/srfi-45) ; primitives for expressing iterative lazy algorithms


(module lshift/common
  (splice-symbol
   interleave-element
   any->string
   enumeration->list
   split-by-equivalence-class
   )

  (import string-io)
  (import srfi-1)
  (import srfi-13)
  (import srfi-19)
  (import srfi-26)
  (import srfi-45) ;; required because sisc-native delay/force are
		   ;; incompatible with srfi-45 delay/force, which are
		   ;; used by the SISCweb SQL interface!

  (import s2j)

  (define-generic-java-methods
    has-more-elements
    next-element)

  (define splice-symbol
    (let ((stx->string (lambda (x)
			 (cond
			  ((string? x) x)
			  ((symbol? x) (symbol->string x))
			  (else (symbol->string (syntax-object->datum x)))))))
      (lambda (scope-stx . parts)
	(datum->syntax-object scope-stx
			      (string->symbol
			       (apply string-append (map stx->string parts)))))))

  (define (interleave-element elt lst)
    (if (null? lst)
	'()
	(drop-right (concatenate (map (cut list <> elt) lst)) 1)))

  (define (any->string x)
    (cond
     ((string? x) x)
     ((number? x) (number->string x))
     ((symbol? x) (symbol->string x))
     (else (call-with-output-string (lambda (port) (write x port))))))

  (define (enumeration->list e . maybe-item-transformer)
    (let ((item-transformer (if (null? maybe-item-transformer)
				values
				(car maybe-item-transformer))))
      (let build-rest ()
	(if (->boolean (has-more-elements e))
	    (let ((first-element (item-transformer (next-element e))))
	      (cons first-element (build-rest)))
	    '()))))

  (define (split-by-equivalence-class pred l)
    (if (null? l)
	'()
	(let loop ((prev (car l))
		   (input (cdr l))
		   (classes '())
		   (class (list (car l))))
	  (if (null? input)
	      (reverse (cons (reverse class) classes))
	      (let ((element (car input)))
		(if (pred prev element)
		    (loop element (cdr input) classes (cons element class))
		    (loop element (cdr input) (cons (reverse class) classes) (list element))))))))
  )
