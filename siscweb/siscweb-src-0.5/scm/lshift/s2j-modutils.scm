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


(module lshift/s2j-modutils
    ((module-define-java-classes module-s2j-definition)
     (module-define-generic-java-methods module-s2j-definition))

  (import s2j)

  (define-syntax module-s2j-definition
    (lambda (stx)
      (syntax-case stx ()
	((self definer clause ...)
	 (with-syntax (((id ...) (map (lambda (clause)
					(syntax-case clause ()
					  ((id other ...) (syntax id))
					  (id (syntax id))))
				      (syntax (clause ...))))
		       (tempnam (datum->syntax-object (syntax self) (gensym))))
	   (syntax
	    (begin (module tempnam (id ...)
		     (definer clause ...))
		   (import tempnam))))))))

  (define-syntax module-define-java-classes
    (syntax-rules ()
      ((_ clause ...)
       (module-s2j-definition define-java-classes clause ...))))

  (define-syntax module-define-generic-java-methods
    (syntax-rules ()
      ((_ clause ...)
       (module-s2j-definition define-generic-java-methods clause ...))))
  )

