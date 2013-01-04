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
;;;     Dan Muresan
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

(require-library 'siscweb/contcentric)
(require-library 'siscweb/response)

(require-extension (srfi 26)) ;; cut

(module siscweb/redirect
  (send-redirect/back send-redirect/suspend
   send-redirect/finish send-redirect/forward)

  (import s2j)

  (define-java-class <URL> |java.net.URL|)
  (define-generic-java-method to-string)
  
  (define (redirect url code)
    (define abs-url
      (let ((base-url (java-new <URL> (request/get-url))))
        (to-string (java-new <URL> base-url url))))
    (response/set-status! code)
    (response/add-header!
     "Location" (response/encode-redirect-url abs-url))
    (response/commit!))
  
  (define (suspend-proc code k-url resume-k)
    (set! resume-k #f)
    (redirect k-url code))
  
  (define (send-redirect/suspend . code)
    (if (null? code) (set! code '(302)))
    (send/suspend (cute suspend-proc (car code) <> <>)))

  (define (send-redirect/back url . code)
    (if (null? code) (set! code '(302)))
    (redirect url (car code)))

  (define (send-redirect/finish url . code)
    (if (null? code) (set! code '(302)))
    (send/finish (cute redir url (car code))))

  (define (send-redirect/forward . code)
    (if (null? code) (set! code '(302)))
    (send/forward (cute suspend-proc (car code) <> <>)))

  )
