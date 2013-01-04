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


(require-library 'siscweb/config)
(require-library 'siscweb/contcentric)
(require-library 'siscweb/error)
(require-library 'siscweb/frame)
(require-library 'siscweb/publish)
(require-library 'siscweb/request)

(module siscweb/sisclet
  (sisclet)

  (import siscweb/config)
  (import siscweb/contcentric)
  (import siscweb/error)
  (import siscweb/frame)
  (import siscweb/publish)
  (import siscweb/request)


  (define (sisclet)
    (call/cc
     (lambda (suspend-k)
       (current-suspend-k suspend-k)
       (set! suspend-k #f)
       (with/fc
        (lambda (m e)
          ((config/get-fc-hook) m e))
        (lambda ()
          (let* ((k-hash (current-k-hash))
                 (resume-k (fetch/k k-hash)))
            (cond (resume-k
                   (current-frame (frame/make (get-request-frame)))
                   (resume-k (current-request)))
                  (else
                   (let ((proc (get-published-proc)))
                     (cond (proc
                            (current-frame (frame/make))
                            (proc (current-request)))
                           (else
                            (send-error/back 404 "Not Found"))))))))))))


  (define (get-published-proc)
    (let ((path-info (request/get-path-info)))
      (and path-info
          (get-published path-info))))
  )
