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
;;; Portions created by the Initial Developer are Copyright (C) 2005-2006
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


(require-library 'siscweb/context)
(require-library 'siscweb/frame)
(require-library 'util/misc)

(module siscweb/k-store
  (ks/clear! ks/get ks/put! ks/get-frame)

  (import s2j)

  (import siscweb/context)
  (import siscweb/frame)
  (import util/misc)

  (define-java-classes
    (<continuation-store-locator> |siscweb.contcentric.ContinuationStoreLocator|))

  (define-generic-java-methods
    clear fetch get-frame get-servlet-context lookup store)

  (define get-k-store
    (let ((k-store #f))
      (lambda (session)
        (when (not k-store)
          (set! k-store (lookup (java-null <continuation-store-locator>)
                                (get-servlet-context session))))
        k-store)))


  ;; stores the given continuation and
  ;; returns a k-url made of the current
  ;; request url with the k-id added/substituted
  (define (ks/put! session k-hash g-hash k ttl current-k-hash)
    (store (get-k-store session) session (->jstring g-hash) (->jstring k-hash) (java-wrap k) (->jlong ttl) (current-frame)))

  ;; fetches the continuation identified by k-hash from the repository
  ;; in the session
  (define (ks/get session k-hash)
    (and k-hash
         (let ((k-wrap (fetch (get-k-store session) session (->jstring k-hash))))
           (if (java-null? k-wrap)
               #f
               (java-unwrap k-wrap)))))


  ;; clears the continuation table
  (define (ks/clear! session)
    (clear (get-k-store session) session))

  (define (ks/get-frame session k-hash)
    (get-frame (get-k-store session) session (->jstring k-hash)))

  )
