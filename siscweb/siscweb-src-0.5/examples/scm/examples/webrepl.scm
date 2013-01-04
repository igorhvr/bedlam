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


(require-library 'siscweb/bindings)
(require-library 'siscweb/xhtml)

(module examples/webrepl
  (webrepl webrepl-not-published)

  (import string-io)

  (import siscweb/bindings)
  (import siscweb/xhtml)

  (define (webrepl req)
    (set! req #f)
    (let loop ((sexp "")
               (out "")
               (result #f)
               (hist '())
               (first-iter? #t))
      (let* ((sexp-new (read-sexp out result sexp hist first-iter?))
             (hist-new (cons sexp-new hist)))
        (with/fc
         (lambda (m e)
           (loop sexp (cdr (assoc 'message m)) e hist-new #f))  ; for errors
         (lambda ()
           (let ((res-and-out (with-input-from-string sexp-new eval-many)))
             (loop sexp-new (cdr res-and-out) (car res-and-out) hist-new #f)))))))


  (define (read-sexp out result sexp hist first-iter?)
    (extract-single-binding
     'sexp
     (get-bindings
      (send-xhtml/suspend
       (lambda (k-url)
         `(*TOP*
           (*PI* xml "version=\"1.0\"")
           (*DTD-INFO/PUBLIC* "html"
                              "-//W3C//DTD XHTML 1.0 Strict//EN"
                              "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
           (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                    (xml:lang "en") (lang "en"))
            (head
             (title "Web REPL")
             (link (@ (href-c "/css/default.css")
                      (rel "stylesheet")
                      (type "text/css"))))
            (body (@ (onload "document.repl.sexp.select(); document.repl.sexp.focus();"))
             (h2 "Web REPL")
             ,@(if first-iter?
                   '()
                   `(,@(if (zero? (string-length out))
                           '()
                           `((p "Output:" (pre ,out))))
                     (p
                      "Result:"
                      (pre
                       ,(with-output-to-string
                          (lambda () (pretty-print result)))))))
             (form (@ (id "repl") (name "repl")
                      (action-e ,k-url) (method "post"))
              (textarea (@ (name "sexp") (cols  80) (rows 12))
               ,(if first-iter? "" sexp))
              (br)
              (input (@ (type "submit") (value "Send")))
              (input (@ (type "reset") (value "Reset"))))
             (p "Previous inputs: ")
             ,@(map (lambda (text) `(div (pre ,text) (hr)))
                    hist)
             (p (a (@ (href-c "/")) "^ Home"))))))))))


  ;; Reads a sequence of s-expressions using (read), evaluates them, saving
  ;; the output to a string, and returns a cons of the last result and the
  ;; accumulated output. Stops upon encountering EOF.
  (define (eval-many)
    (let* ((result #f)
           (out (with-output-to-string
                  (lambda ()
                    (let read-loop ()
                      (let ((sexp1 (read)))
                        (unless (eof-object? sexp1)
                          (begin
                            (set! result (eval sexp1))
                            (read-loop)))))))))
      (cons result out)))


  (define (webrepl-not-published req)
    (send-xhtml/back
     `(*TOP*
       (*PI* xml "version=\"1.0\"")
       (*DTD-INFO/PUBLIC* "html"
                          "-//W3C//DTD XHTML 1.0 Strict//EN"
                          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
       (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                (xml:lang "en") (lang "en"))
        (head
         (title "Web REPL Not Published")
         (link (@ (href-c "/css/default.css")
                  (rel "stylesheet")
                  (type "text/css"))))
        (body
         (h2 "WebREPL Not Published")
         (p "The web REPL is not currently published. This is the default, as it would not be prudent to ship these examples with a REPL publicly accessible via HTTP.")
         (p "The web REPL can be activated by connecting to the network REPL on localhost:5156 and issuing:")
         (div (@ (class "code"))  "(publish \"/webrepl/*\" 'webrepl)")
         (p "If these examples are from the SISCweb distribution, as opposed to being built from source, this latter REPL is also disabled for security reasons. It can be activated by uncommenting the appropriate entry in the WEB-INF/web.xml file.")
         (p (a (@ (href-c "/")) "^ Home")))))))

 )

