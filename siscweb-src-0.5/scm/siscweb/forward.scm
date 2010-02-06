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

(require-library 'siscweb/bindings)
(require-library 'siscweb/contcentric)
(require-library 'siscweb/request)
(require-library 'siscweb/response)
(require-library 'util/misc)

(module siscweb/forward
  (send-forward/suspend send-forward/finish
   send-forward/forward send-forward/back
   forward/store! forward/dynenv/store!)

  (import s2j)
  (import type-system)

  (import srfi-16)

  (import siscweb/bindings)
  (import siscweb/contcentric)
  (import siscweb/request)
  (import siscweb/response)
  (import util/misc)

  (define-java-classes
    (<sisc-http-servlet-request> |siscweb.web.SISCHttpServletRequest|))


  (define (make-bound-request request bindings)
    (if (null? bindings)
        request
        (java-new <sisc-http-servlet-request>
                  request
                  (if (list? bindings)
                      (alist->bindings bindings)
                      bindings))))

  (define (forward url bindings)
    ((request/get-dispatcher url)
     (make-bound-request (current-request) bindings)
     (current-response)))

  (define send-forward/suspend
    (case-lambda
      ((url)
       (send-forward/suspend url '()))
      ((url bindings)
       (send/suspend
        (lambda (k-url resume-k)
          (request/set-java-attribute! "siscweb.kURL" (->jstring k-url))
          (forward url bindings))))))

  (define send-forward/finish
    (case-lambda
      ((url)
       (send-forward/finish url '()))
      ((url bindings)
       (send/finish
        (lambda ()
          (forward url bindings))))))


  (define send-forward/forward
    (case-lambda
      ((url)
       (send-forward/forward url '()))
      ((url bindings)
       (send/forward
        (lambda (k-url resume-k)
          (request/set-java-attribute! "siscweb.kURL" (->jstring k-url))
          (forward url bindings))))))

  (define send-forward/back
    (case-lambda
      ((url)
       (send-forward/back url '()))
      ((url bindings)
       (send/back
        (lambda ()
          (forward url bindings))))))



  (define forward/store!
    (case-lambda
      ((proc-or-c-url)
       (if (procedure? proc-or-c-url)
           (store/k! proc-or-c-url)
           (response/encode-url (string-append
                                 (request/get-context-path)
                                 proc-or-c-url))))
      ((proc-or-c-url bindings)
       (if (procedure? proc-or-c-url)
           (forward/store!
            (lambda (request)
              (proc-or-c-url (make-bound-request request bindings))))
           (send-forward/back proc-or-c-url bindings)))))

  (define (capture-dynenv proc)
    (let/cc k-esc
      (proc
       (let/cc k-res
         (k-esc k-res)))))

  (define (forward/dynenv/store! proc . rest)
    (if (procedure? proc)
        (apply forward/store! (cons (capture-dynenv proc) rest))
        (forward/store! (cons proc rest))))
  )
