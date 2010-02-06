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

(require-library 'siscweb/contcentric)
(require-library 'siscweb/response)
(require-library 'sxml/ehtml)
(require-library 'sxml/markup)

(module siscweb/html
  (send-html/suspend send-html/finish
   send-html/forward send-html/back)

  (import srfi-16)

  (import siscweb/contcentric)
  (import siscweb/response)
  (import sxml/ehtml)
  (import sxml/markup)


  (define (send-html header-alst page)
    (when (not (assoc "Content-Type" header-alst))
      (response/set-content-type! "text/html"))
    (response/add-headers! header-alst)
    (display (sxml->string page)
             (response/open-output-port)))

  (define send-html/back
    (case-lambda
      ((page)
       (send-html/back '() page))
      ((header-alst page)
       (send/back
        (lambda ()
          (send-html
           header-alst
           (ehtml->html page (lambda (x) #f))))))))

  (define send-html/finish
    (case-lambda
      ((page)
       (send-html/finish '() page))
      ((header-alst page)
       (send/finish
        (lambda ()
          (send-html
           header-alst
           (ehtml->html page (lambda (x) #f))))))))

  (define send-html/suspend
    (case-lambda
      ((page-proc)
       (send-html/suspend '() page-proc))
      ((header-alst page-proc)
       (send/suspend
        (lambda (k-url resume-k)
          (send-html
           header-alst
           (ehtml->html (page-proc k-url) resume-k)))))))

  (define send-html/forward
    (case-lambda
      ((page-proc)
       (send-html/forward '() page-proc))
      ((header-alst page-proc)
       (send/forward
        (lambda (k-url resume-k)
          (send-html
           header-alst
           (ehtml->html (page-proc k-url) resume-k)))))))
  )
