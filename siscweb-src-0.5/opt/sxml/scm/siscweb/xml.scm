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

(require-library 'siscweb/config)
(require-library 'siscweb/contcentric)
(require-library 'siscweb/response)
(require-library 'ssax/ssax)
(require-library 'sxml/sxml-to-xml)

(module siscweb/xml
  (send-xml/back send-xml/finish
   send-xml/suspend send-xml/forward
   read-xml)

  (import string-io)

  (import srfi-16)

  (import siscweb/config)
  (import siscweb/contcentric)
  (import siscweb/response)
  (import ssax)
  (import sxml/sxml-to-xml)


  (define (send-xml header-alst doc)
    (when (not (assoc "Content-Type" header-alst))
      (response/set-content-type! "application/xml"))
    (response/add-headers! header-alst)
    (if (config/sxml-formatted-output?)
        (sxml/display doc (response/open-output-port))
        (sxml/write doc (response/open-output-port))))

  (define send-xml/back
    (case-lambda
      ((doc)
       (send-xml/back '() doc))
      ((header-alst doc)
       (send/back
        (lambda ()
          (send-xml header-alst doc))))))

  (define send-xml/finish
    (case-lambda
      ((doc)
       (send-xml/finish '() doc))
      ((header-alst doc)
       (send/finish
        (lambda ()
          (send-xml header-alst doc))))))


  (define send-xml/suspend
    (case-lambda
      ((doc-proc)
       (send-xml/suspend '() doc-proc))
      ((header-alst doc-proc)
       (send/suspend
        (lambda (k-url resume-k)
          (send-xml header-alst (doc-proc k-url)))))))

  (define send-xml/forward
    (case-lambda
      ((doc-proc)
       (send-xml/forward '() doc-proc))
      ((header-alst doc-proc)
       (send/forward
        (lambda (k-url resume-k)
          (send-xml header-alst (doc-proc k-url)))))))

  (define read-xml
    (case-lambda
      ((namespaces)
       (read-xml namespaces (current-input-port)))
      ((namespaces port)
       (SSAX:XML->SXML port namespaces))))
  )
