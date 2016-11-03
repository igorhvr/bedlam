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
;;; Portions created by the Initial Developer are Copyright (C) 2005
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


(require-library 'siscweb/html)

(module error/fc-handler
  (fc-handler)

  (import debugging)
  (import string-io)

  (import siscweb/html)


  (define (fc-handler m e)
    (send-html/back
     `(html
       (head
        (title "Error")
        (link (@ (href-c "/error-css/default.css")
                 (rel "stylesheet")
                 (type "text/css")))
        )
       (body
        (table (@ (style "width: 100%; background-color: Black;"))
         (tr
          (td (@ (style "padding: 0;"))
           (div  (@ (style "text-align: center"))
            (img (@ (src-c "/error-images/gurumeditation.gif") (border 0)))))))

        (h3 "Details")
        (table
         ,@(map (lambda (pair)
                  `(tr
                    (td (@ (style "font-weight: bold;"))
                     ,(symbol->string (car pair)))
                    (td ,(with-output-to-string
                           (lambda ()
                             (display (cdr pair)))))))
                m))

        (h3 "Stack Trace")
        (pre
         ,(with-output-to-string
            (lambda ()
              (print-stack-trace e))))))))
  )
