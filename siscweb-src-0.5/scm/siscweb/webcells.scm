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
;;; Portions created by the Initial Developer are Copyright (C) 2007
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

(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity

(require-library 'siscweb/frame)

(module siscweb/webcells
  (make-cell make-cell-parameter
   cell-ref cell-shadow
   (webcell/make make-cell)
   (webcell/make-parameter make-cell-parameter)
   webcell/ref webcell/set!)

  (import srfi-16)

  (import siscweb/frame)


  (define make-cell
    (case-lambda
      ((default-value)
       (make-cell (gensym) default-value))
      ((name default-value)
       (cons name default-value))))

  (define-syntax webcell/make
    (syntax-rules ()
      ((_ name default-value)
       (make-cell (quote name) default-value))
      ((_ default-value)
       (make-cell default-value))))

  (define (cell-name cell)
    (car cell))

  (define (cell-default-value cell)
    (cdr cell))

  (define make-cell-parameter
    (case-lambda
     ((default-value)
      (make-cell-parameter (gensym) default-value))
     ((name default-value)
      (let ((cell (make-cell name default-value)))
        (case-lambda
         (()
          (webcell/ref cell))
         ((value)
          (cell-set! cell value)))))))

  (define-syntax webcell/make-parameter
    (syntax-rules ()
      ((_ name default-value)
       (make-cell-parameter (quote name) default-value))
      ((_ default-value)
       (make-cell-parameter default-value))))

  (define (cell-ref cell)
    (or (frame/lookup-value (cell-name cell))
        (cell-default-value cell)))

  (define (webcell/ref cell)
    (cell-ref cell))

  (define (cell-set! cell value)
    (let ((name (cell-name cell)))
      (frame/put-value! name value)))

  (define (cell-shadow cell value)
    (cell-put! cell value))

  (define (webcell/set! name value)
    (cell-set! name value))
  )
