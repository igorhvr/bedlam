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
(require-library 'sisc/libs/srfi/srfi-39) ; parameter objects

(require-library 'siscweb/k-hash)
(require-library 'siscweb/k-store)
(require-library 'siscweb/request)
(require-library 'siscweb/response)
(require-library 'siscweb/session)
(require-library 'util/misc)
(require-library 'util/regexp)

(module siscweb/contcentric
  (send/back send/finish send/forward send/suspend
   current-suspend-k current-k-hash
   adjust-timeout!
   clear/k! fetch/k store/k!
   get-request-frame)

  (import srfi-16)
  (import srfi-39)

  (import siscweb/k-hash)
  (import siscweb/k-store)
  (import siscweb/request)
  (import siscweb/response)
  (import siscweb/session)
  (import util/misc)
  (import util/regexp)

  ;; sends a response and stops; unlike send/finish, the continuation
  ;; table is not touched
  (define send/back
    (case-lambda
      ((header-alst send-proc)
       (response/add-headers! header-alst)
       (send/back send-proc))
      ((send-proc)
       (send-proc)
       ((current-suspend-k) #f))))

  ;; sends a response and stops; all continuations prior to this one
  ;; are cleared
  (define send/finish
    (case-lambda
      ((header-alst send-proc)
       (response/add-headers! header-alst)
       (send/finish send-proc))
      ((send-proc)
       (clear/k!)
       (send-proc)
       ((current-suspend-k) #f))))


  ;; sends a response and suspends, but clears the continuation table
  ;; first
  (define send/forward
    (case-lambda
      ((header-alst send-proc)
       (response/add-headers! header-alst)
       (send/forward send-proc))
      ((send-proc)
       (clear/k!)
       (send/suspend send-proc))))

  ;; sends a response and suspends
  (define send/suspend
    (case-lambda
      ((header-alst send-proc)
       (response/add-headers! header-alst)
       (send/suspend send-proc))
      ((send-proc)
       (let/cc resume-k
         (let ((k-url (store/k! resume-k)))
           (send-proc k-url resume-k)
           ((current-suspend-k) #f))))))


  (define (fetch/k k-hash)
    (ks/get (request/get-session) k-hash))

  (define (store/k! k)
    (define jsid-re (make-regexp "(\\A.*)(;jsessionid=\\p{Alnum}+)(.*\\z)"))

    (define (encode-url url)
      (response/encode-url (remove-jsessionid url)))

    ;; tomcat will happily add a second jsessionid element
    ;; so we must remove the first if present
    (define (remove-jsessionid url)
      (if (regexp-match? jsid-re url)
          (regexp-replace jsid-re url "$1$3")
          url))

    (when (not (current-g-hash))
      (current-g-hash (make-random-hash)))
    (let ((k-hash (make-random-hash)))
      (ks/put! (request/get-session) k-hash (current-g-hash)
               k (current-k-ttl) (current-k-hash))
      (encode-url (make-k-url (request/get-uri) k-hash))))


  (define (clear/k!)
    (ks/clear! (request/get-session)))

  (define (get-request-frame)
    (ks/get-frame (current-session) (current-k-hash)))


  (define (current-k-hash)
    (extract-k-hash (request/get-uri)))

  (define (adjust-timeout! ttl) ; seconds
    (current-k-ttl ttl))


  (define current-suspend-k (make-parameter #f))
  (define current-g-hash (make-parameter #f))
  (define current-k-ttl (make-parameter -1))
  )
