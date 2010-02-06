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

(require-library 'siscweb/bindings)
(require-library 'siscweb/xhtml)
(require-library 'sxml/dotml)

(module examples/graphpad
  (graphpad)

  (import string-io)
  (import siscweb/bindings)
  (import siscweb/xhtml)
  (import sxml/dotml)


  (define sps-graph-string
    (string-append
     "(graph (@ (id \"G\"))\n"
     "  (node (@ (id \"c\") (label \"scissors\") (href \"http://www.google.com/search?q=scissors\")))\n"
     "  (node (@ (id \"p\") (label \"paper\") (href \"http://www.google.com/search?q=paper\")))\n"
     "  (node (@ (id \"s\") (label \"stone\") (href \"http://www.google.com/search?q=stone\")))\n"
     "  (edge (@ (from \"c\") (to \"p\") (label \"cut\")))\n"
     "  (edge (@ (from \"p\") (to \"s\") (label \"wraps\")))\n"
     "  (edge (@ (from \"s\") (to \"c\") (label \"breaks\")))))"))


  (define (graphpad req)
    (let loop ((layout "dot")
               (format "gif")
               (graph-string sps-graph-string))
      (let-bindings ((layout "layout")
                     (format "format")
                     (graph-string "graph"))
          (send-main-page layout format graph-string)
        (loop layout format graph-string))))


  ;; (layout format graph) => bindings
  (define (send-main-page layout format graph-string)
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
            (title "Graphpad")
            (link (@ (href-c "/css/default.css")
                     (rel "stylesheet")
                     (type "text/css"))))
           (body
            (h3 "Graphpad")
            ,(make-embedded-graph layout format graph-string)
            (form (@ (action-e ,k-url) (method "post"))
             (div (@ (style "text-align: center;"))
              (textarea (@ (id "graph") (name "graph") (cols "80") (rows "12"))
               ,graph-string)
              (br)
              (input (@ (type "submit") (value "Update")))
              "Layout:"
              (select (@ (id "layout") (name "layout"))
               ,@(map (lambda (l)
                        `(option (@ (value ,l)
                                    (selected ,(if (equal? l layout) "yes" #f)))
                                 ,l))
                      '("dot" "neato" "twopi" "circo" "fdp")))
              "Format:"
              (select (@ (id "format") (name "format"))
               ,@(map (lambda (f)
                        `(option (@ (value ,f)
                                    (selected ,(if (equal? f format) "yes" #f)))
                                 ,f))
                      '("gif" "jpg" "png")))
              (a (@ (href-p ,(lambda (req)
                               (set! req #f)
                               (send-graph-source graph-string)))
                    (target "graph-source"))
               "view graph source..."))
             (p (@ (style "font-style: italic;"))
              "Note: If you are unable to see a graph image, you will need to install "
              (a (@ (href "http://www.graphviz.org")) "Graphviz")
              ", and place the executables in the system path."))
            (p (a (@ (href-c "/")) "^ Home")))))))))


  (define (make-embedded-graph layout format graph-string)
    `(div (@ (style "text-align: center;"))
      (object (@ (type "graphviz") (layout ,layout) (format ,format)
                 (style "text-align: center;"))
              ,(with-input-from-string graph-string read))))


  (define (send-graph-source graph-string)
    (send-xhtml/back
     `(*TOP*
       (*DTD-INFO/PUBLIC* "html"
                          "-//W3C//DTD XHTML 1.0 Strict//EN"
                          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
       (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                (xml:lang "en") (lang "en"))
        (head
         (title "Graph Source")
         (link (@ (href-c "/css/default.css")
                  (rel "stylesheet")
                  (type "text/css"))))
        (body
         ,(with/fc
           (lambda (m e)
             `(p "Invalid DotML"))
           (lambda ()
             `(p ,(dotml->dot (with-input-from-string graph-string read))))))))))

  )
