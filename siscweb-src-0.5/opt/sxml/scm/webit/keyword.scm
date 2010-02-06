;;; WebIt! Collection
;;; Copyright (c) 2000-2005 Jim Bender
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Contributor(s):
;;; Alessandro Colomba: SISC port (2006)

;; a DSSSL-like keywords

;; missing from SISC port: declare-keyword macro

(require-library 'sisc/libs/srfi/srfi-9) ; record types

(module webit/keyword
  (make-dsssl-keyword dsssl-keyword? dsssl-keyword-tag dsssl-keyword-print-tag dsssl-keyword-ns-uri)
  ;; declare-keyword

  (import srfi-9)

  (define-record-type dsssl-keyword
    (make-dsssl-keyword tag print-tag ns-uri)
    dsssl-keyword?
    (tag dsssl-keyword-tag)
    (print-tag dsssl-keyword-print-tag)
    (ns-uri dsssl-keyword-ns-uri))


;   (define-syntax (declare-keyword stx)
;     (syntax-case stx ()
;       [(declare-dsssl-keyword tag)
;        (with-syntax ((kwd (datum->syntax-object (syntax tag)
;                                                 (string->symbol
;                                                  (string-append
;                                                   (symbol->string (syntax-object->datum (syntax tag)))
;                                                   ":"))))
;                      (kwd-syntax (datum->syntax-object (syntax tag)
;                                                        (string->symbol
;                                                         (string-append
;                                                          (symbol->string (syntax-object->datum (syntax tag)))
;                                                          ":attribute")))))
;          (syntax
;           (begin
;             (define-syntax kwd-syntax 'tag)
;             (define-syntax (kwd stx)
;               (syntax-case stx ()
;                 (name
;                  (identifier? (syntax name))
;                  (syntax (make-dsssl-keyword (quote tag)
;                                              (quote tag)
;                                              #f))))))))]
;       [(declare-dsssl-keyword ptag ns-uri tag)
;        (with-syntax ((kwd (datum->syntax-object (syntax ptag)
;                                                 (string->symbol
;                                                  (string-append
;                                                   (symbol->string (syntax-object->datum (syntax ptag)))
;                                                   ":"))))
;                      (kwd-syntax (datum->syntax-object (syntax ptag)
;                                                        (string->symbol
;                                                         (string-append
;                                                          (symbol->string (syntax-object->datum (syntax ptag)))
;                                                          ":attribute"))))
;                      (ename (datum->syntax-object (syntax ptag)
;                                                   (let ((ns (syntax-object->datum (syntax ns-uri)))
;                                                         (lname (syntax-object->datum (syntax tag))))
;                                                     (if ns
;                                                         (string->symbol
;                                                          (string-append
;                                                           (symbol->string ns)
;                                                           ":"
;                                                           (symbol->string lname)))
;                                                         lname)))))
;          (syntax
;           (begin
;             (define-syntax kwd-syntax 'ename)
;             (define-syntax (kwd stx)
;               (syntax-case stx ()
;                 (name
;                  (identifier? (syntax name))
;                  (syntax (make-dsssl-keyword (quote ename)
;                                              (quote tag)
;                                              (quote ns-uri)))))))))]))

  )
