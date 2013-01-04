;;; The contents of this file are subject to the Mozilla Public License Version
;;; 1.1 (the "License"); you may not use this file except in compliance with
;;; the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS" basis,
;;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;; for the specific language governing rights and limitations under the
;;; License.
;;;
;;; The Original Code is SISCweb.
;;;
;;; The Initial Developer of the Original Code is Alessandro Colomba.
;;; Portions created by the Initial Developer are Copyright (C) 2005-2007
;;; Alessandro Colomba. All Rights Reserved.
;;;
;;; Contributor(s):
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either the GNU General Public License Version 2 or later (the "GPL"), or
;;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;;; in which case the provisions of the GPL or the LGPL are applicable instead
;;; of those above. If you wish to allow use of your version of this file only
;;; under the terms of either the GPL or the LGPL, and not to allow others to
;;; use your version of this file under the terms of the MPL, indicate your
;;; decision by deleting the provisions above and replace them with the notice
;;; and other provisions required by the GPL or the LGPL. If you do not delete
;;; the provisions above, a recipient may use your version of this file under
;;; the terms of any one of the MPL, the GPL or the LGPL.

(require-library 'sisc/libs/srfi/srfi-1) ; list library

(module util/misc
  (let/cc typecase
   ->jbool
   enumeration/map enumeration/for-each
   iterator/map iterator/for-each
   jlist/map jlist/for-each
   jmap/map jmap/for-each
   jarray/map jarray/for-each
   alist->lstring)

  (import s2j)
  (import string-io)
  (import type-system)

  (import srfi-1)

  (define-java-classes
    (<jbool> |java.lang.Boolean|))

  (define-generic-java-methods entry-set get-key get-value iterator has-more-elements has-next next next-element)


  (define-syntax let/cc
    (syntax-rules ()
      ((_ k body ...)
       (call/cc (lambda (k) body ...)))))


  (define-syntax typecase
    (syntax-rules (else)
      ((_ value
          ((type ...) expr ...) ...)
       (cond ((or (instance-of? value type) ...)
              expr ...)
             ...))
      ((_ value
          ((type ...) expr ...)
          ...
          (else expr1 ...))
       (cond ((or (instance-of? value type) ...)
              expr ...)
             ...
             (else expr1 ...)))))


  (define (->jbool x)
    (define jtrue ((generic-java-field-accessor '|TRUE|) (java-null <jbool>)))
    (define jfalse ((generic-java-field-accessor '|FALSE|) (java-null <jbool>)))

    (if x jtrue jfalse))

  (define (enumeration/map proc enum)
    (define (M l)
      (if (->boolean (has-more-elements enum))
        (let ((x (proc (next-element enum))))
          (M (cons x l)))
        (reverse l)))

    (M '()))

  (define (enumeration/for-each proc enum)
    (define (F)
      (when (->boolean (has-more-elements enum))
        (proc (next-element enum))
        (F)))

    (F))


  (define (iterator/map proc iter)
    (define (M l)
      (if (->boolean (has-next iter))
        (let ((x (proc (next iter))))
          (M (cons x l)))
        (reverse l)))

    (M '()))

  (define (iterator/for-each proc iter)
    (define (F)
      (when (->boolean (has-next iter))
        (proc (next iter))
        (F)))

    (F))

  (define (jarray/map proc array)
    (define (R i acc)
      (if (>= i 0)
          (R (- i 1) (cons (proc (java-array-ref array i)) acc))
          acc))
    (R (- (java-array-length array) 1) '()))

  (define (jarray/for-each proc array)
    (define (R i)
      (when (>= i 0)
        (proc (java-array-ref array i))
        (R (- i 1))))

    (R (- (java-array-length array) 1)))


  (define (jlist/map proc jlist)
    (iterator/map proc (iterator jlist)))

  (define (jlist/for-each proc jlist)
    (iterator/for-each proc (iterator jlist)))

  (define (jmap/map proc jmap)
    (iterator/map
     (lambda (entry)
       (let ((jkey (get-key entry))
             (jvalue (get-value entry)))
         (proc jkey jvalue)))
     (iterator (entry-set jmap))))

  (define (jmap/for-each proc jmap)
    (iterator/for-each
     (lambda (entry)
       (let ((jkey (get-key entry))
             (jvalue (get-value entry)))
         (proc jkey jvalue)))
     (iterator (entry-set jmap))))


  (define (alist->lstring alist isep osep)
    (fold-right
     (lambda (pair str)
       (format "~a~a~a~a"
               (car pair)
               isep
               (cadr pair)
               (if (> (string-length str) 0)
                   (string-append osep str)
                   "")))
     ""
     alist))

  )
