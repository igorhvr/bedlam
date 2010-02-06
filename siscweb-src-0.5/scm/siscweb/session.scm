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


(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity
(require-library 'sisc/libs/srfi/srfi-18) ; multithreading support
(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures

(require-library 'siscweb/request)
(require-library 'util/misc)

(module siscweb/session
  (current-session
   session/get-creation-time session/get-id
   session/get-java-attribute session/get-java-attribute-names
   session/get-last-accessed-time session/get-max-inactive-interval
   session/get-servlet-context session/invalidate!
   session/make-parameter session/new?
   session/remove-java-attribute! session/set-java-attribute!
   session/set-max-inactive-interval! current-session)

  (import s2j)

  (import srfi-16)
  ;; time? is defined both in srfi-18 and srfi-19
  (import* srfi-18 make-mutex mutex-lock! mutex-unlock!)
  (import srfi-19)

  (import siscweb/request)
  (import util/misc)

  (define-generic-java-methods
    (jget-attribute |getAttribute|)
    (jget-attribute-names |getAttributeNames|)
    (jget-creation-time |getCreationTime|)
    (jget-max-inactive-interval |getMaxInactiveInterval|)
    (jget-servlet-context |getServletContext|)
    (jget-id |getId|)
    (jget-last-accessed-time |getLastAccessedTime|)
    (jinvalidate |invalidate|)
    (jis-new |isNew|)
    (jremove-attribute |removeAttribute|)
    (jset-attribute |setAttribute|)
    (jset-max-inactive-interval |setMaxInactiveInterval|))


  (define (session/get-scheme-attribute name)
    (let ((jvalue (session/get-java-attribute name)))
      (if (java-null? jvalue)
          #f
          (java-unwrap jvalue))))

  (define (session/set-scheme-attribute! name value)
    (session/set-java-attribute! name (if value (java-wrap value) jnull))
    (void))

  (define (session/remove-scheme-attribute! name)
    (session/remove-java-attribute! name))

  (define session/make-parameter
    (case-lambda
     ((name)
      (session/make-parameter name #f))
     ((name thread-safe?)
      (if thread-safe?
          (let ((mutex (make-mutex)))
            (case-lambda
             (()
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda () (session/get-scheme-attribute name))
               (lambda () (mutex-unlock! mutex))))
             ((value)
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda ()
                 (if value
                     (begin
                       (session/set-scheme-attribute! name value)
                       value)
                     (session/remove-scheme-attribute! name)))
               (lambda () (mutex-unlock! mutex))))))
          (case-lambda
           (() (session/get-scheme-attribute name))
           ((value)
            (if value
                (begin
                  (session/set-scheme-attribute! name value)
                  value)
                (session/remove-scheme-attribute! name))))))))

  (define (session/get-java-attribute name)
    (jget-attribute (current-session) (->jstring name)))

  (define (session/remove-java-attribute! name)
    (jremove-attribute (current-session) (->jstring name))
    (void))

  (define (session/set-java-attribute! name java-object)
    (jset-attribute (current-session) (->jstring name) java-object)
    (void))

  (define (session/get-java-attribute-names)
    (enumeration/map
     ->string
     (jget-attribute-names (current-session))))


  (define (session/get-creation-time)
    (let ((ms (->number (jget-creation-time (current-session)))))
      (make-time 'time-monotonic 0 (quotient ms 1000))))

  (define (session/get-last-accessed-time)
    (let ((ms (->number (jget-last-accessed-time (current-session)))))
      (make-time 'time-monotonic 0 (quotient ms 1000))))

  (define (session/get-id)
    (->string (jget-id (current-session))))

  (define (session/get-servlet-context)
    (jget-servlet-context (current-session)))

  (define (session/get-max-inactive-interval)
    (->number (jget-max-inactive-interval (current-session))))

  (define (session/set-max-inactive-interval! seconds)
    (jset-max-inactive-interval (current-session) (->jint seconds))
    (void))


  (define (session/invalidate!)
    (jinvalidate (current-session))
    (void))

  (define (session/new?)
    (->boolean (jis-new (current-session))))


  (define (current-session)
    (request/get-session))
)
