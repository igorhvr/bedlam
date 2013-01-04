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

(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity

(module siscweb/frame
  (frame/make frame/get-id
   frame/lookup-value frame/put-value!
   current-frame)

  (import s2j)

  (import srfi-16)

  (define-java-classes
   (<frame> |siscweb.contcentric.Frame|))

  (define-generic-java-methods
   (jget-id |getId|)
   (jlookup |lookup|)
   (jput |set|)
   (jget-frame |getFrame|))


  (define frame/make
    (case-lambda
     (()
      (java-new <frame> (->jstring (gensym))))
     ((parent)
      (java-new <frame> (->jstring (gensym)) parent))))

  (define (frame/lookup-value name)
    (let ((frame (current-frame)))
      (and frame
           (let ((jvalue (jlookup frame (->jstring name))))
             (and (not (java-null? jvalue))
                  (java-unwrap jvalue))))))

  (define (frame/put-value! name value)
    (jput (current-frame) (->jstring name) (java-wrap value)))

  (define (frame/get-id frame)
    (->symbol (jget-id frame)))

   (define current-frame (make-parameter #f))
  )
