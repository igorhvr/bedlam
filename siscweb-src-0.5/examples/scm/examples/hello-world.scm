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

;;; This is the classic hello world program with a twist; messages in different
;;; languages are sent to the browser from inside the for-each form; this is
;;; possibly the simplest illustration of continuation-based web programming style.

(require-library 'siscweb/xhtml)

(module examples/hello-world
  (hello-world)

  (import siscweb/xhtml)

  (define messages '("Hello, world!" "Salve, mundo!" "Hallo, Welt!" "Salve, mondo!" "Bonjour, monde!" "Hola, mundo!"))

  (define (hello-world request)
    (let loop ()
      (for-each
       (lambda (message)
         (send-xhtml/suspend
          (make-hello-world-page message)))
       messages)
      (loop)))


  (define (make-hello-world-page message)
    (lambda (k-url)
      `(*TOP*
        (*PI* xml "version=\"1.0\"")
        (*DTD-INFO/PUBLIC* "html"
                           "-//W3C//DTD XHTML 1.0 Strict//EN"
                           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") (lang "en"))
         (head
          (title ,message)
          (link (@ (href-c "/css/default.css")
                   (rel "stylesheet")
                   (type "text/css"))))
         (body
          (h3 "Polyglot hello world")
          (p ,message)

          (p (a (@ (href-e ,k-url)) "Next language >"))
          (p (a (@ (href-e ,k-url) (target "_blank")) "Next language in new window >"))
          (p (a (@ (href-c "/")) "^ Home")))))))
  )
