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

(require-library 'siscweb/bindings)
(require-library 'siscweb/forward)
(require-library 'siscweb/graphviz)
(require-library 'siscweb/request)
(require-library 'siscweb/response)
(require-library 'sxml/sxml-match)
(require-library 'util/misc)

(module sxml/ehtml
  (ehtml->html)

  (import string-io)

  (import srfi-16)

  (import siscweb/bindings)
  (import siscweb/forward)
  (import siscweb/graphviz)
  (import siscweb/request)
  (import siscweb/response)
  (import sxml-matcher)
  (import util/misc)


  (define (ehtml->html html resume-k)
    (define (make-graphviz layout id fmt attrs graph)
      (define (make-graph-dispatcher)
        (forward/store!
         (lambda (request)
           (set! request #f)
           (send-graphviz/back layout fmt graph))))

      (define (make-image-map)
        (with/fc
         (lambda (m e) (display m))
         (lambda ()
           `(div
             (*VERBATIM*
              ,(let ((str-out (open-output-string)))
                 (graphviz/write layout "cmapx" graph str-out)
                 (get-output-string str-out)))
             (img (@ (src ,(make-graph-dispatcher))
                     (border 0)
                     (type ,(graphviz/get-mime-type fmt))
                     (usemap ,(string-append "#" id)) . ,attrs))))))

      (define (make-object)
        `(object (@ (data ,(make-graph-dispatcher)) (type ,(graphviz/get-mime-type fmt)) . ,attrs)))

      (if (member fmt '("gif" "jpg" "png"))
          (make-image-map)
          (make-object)))

    (define make-context-url
      (case-lambda
        ((url)
         (response/encode-url (string-append (request/get-context-path) url)))
        ((url bindings)
         (if (null? bindings)
             (make-context-url url)
             (forward/store! url bindings)))
        ((url bindings anchor)
         (make-context-url (make-anchor-url url anchor)))))

    (define (make-anchor-url url anchor)
      (if anchor (format "~a#~a" url anchor) url))

    (define (T html)
      (import sxml-accessors)

      (define (xml-element? elt)
        (and (pair? elt)
             (symbol? (car elt))))

      (sxml-match html
        ;; @href-p [@bindings]
        ((a (@ (href-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(a (@ (href ,(make-anchor-url (forward/store! proc bindings) anchor)) . ,rest)
             ,x ...))
        ((link (@ (href-p ,proc) (bindings (,bindings ())) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(link (@ (href ,(forward/store! proc bindings)) . ,rest)
             ,x ...))
        ((form (@ (action-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(form (@ (action ,(make-anchor-url (forward/store! proc bindings) anchor)) . ,rest)
             ,x ...))
        ((img (@ (src-p ,proc) (bindings (,bindings ())) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(img (@ (src ,(forward/store! proc bindings)) . ,rest)
             ,x ...))
        ((script (@ (src-p ,proc) (bindings (,bindings ())) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(script (@ (src ,(forward/store! proc bindings)) . ,rest)
             ,x ...))
        ((object (@ (data-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(object (@ (data ,(make-anchor-url (forward/store! proc bindings))) . ,rest)
             ,x ...))
        ((iframe (@ (src-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(iframe (@ (src ,(make-anchor-url (forward/store! proc bindings))) . ,rest)
             ,x ...))
        ((node (@ (href-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(node (@ (href ,(make-anchor-url (forward/store! proc bindings) anchor)) . ,rest)
             ,x ...))
        ((edge (@ (href-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(edge (@ (href ,(make-anchor-url (forward/store! proc bindings) anchor)) . ,rest)
             ,x ...))
        ((record (@ (href-p ,proc) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (procedure? proc))
         `(record (@ (href ,(make-anchor-url (forward/store! proc bindings) anchor)) . ,rest)
             ,x ...))


        ;; @bindings
        ((a (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(a (@ (href ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((link (@ (bindings ,bindings) . ,rest) ,[x] ...)
         `(link (@ (href ,(forward/store! resume-k bindings)) . ,rest)
             ,x ...))
        ((form (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(form (@ (action ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((img (@ (bindings ,bindings) . ,rest) ,[x] ...)
         `(img (@ (src ,(forward/store! resume-k bindings)) . ,rest)
             ,x ...))
        ((script (@ (bindings ,bindings) . ,rest) ,[x] ...)
         `(script (@ (src ,(forward/store! resume-k bindings)) . ,rest)
             ,x ...))
        ((object (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(object (@ (data ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((iframe (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(iframe (@ (src ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((node (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(node (@ (href ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((edge (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(edge (@ (href ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))
        ((record (@ (bindings ,bindings) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         `(record (@ (href ,(make-anchor-url (forward/store! resume-k bindings) anchor)) . ,rest)
             ,x ...))


        ;; @href-e
        ((a (@ (href-e ,url) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(a (@ (href ,(response/encode-url (make-anchor-url url anchor))) . ,rest) ,x ...))
        ((link (@ (href-e ,url) . ,rest) ,[x] ...)
         (guard (string? url))
         `(link (@ (href ,(response/encode-url url)) . ,rest) ,x ...))
        ((form (@ (action-e ,url) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(form (@ (action ,(response/encode-url (make-anchor-url url anchor))) . ,rest) ,x ...))
        ((img (@ (src-e ,url) . ,rest))
         (guard (string? url))
         `(img (@ (src ,(response/encode-url url)) . ,rest)))
        ((script (@ (src-e ,url) . ,rest) ,[x] ...)
         (guard (string? url))
         `(script (@ (src ,(response/encode-url url)) . ,rest) ,x ...))
        ((object (@ (data-e ,url) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(object (@ (data ,(response/encode-url (make-anchor-url url anchor))) . ,rest) ,x ...))
        ((iframe (@ (src-e ,url) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(iframe (@ (src ,(response/encode-url (make-anchor-url url anchor))) . ,rest) ,x ...))
        ((node (@ (href-e ,url) (anchor (,anchor #f)) . ,rest))
         (guard (string? url))
         `(node (@ (href ,(response/encode-url (make-anchor-url url anchor))) . ,rest)))
        ((edge (@ (href-e ,url) (anchor (,anchor #f)) . ,rest))
         (guard (string? url))
         `(edge (@ (href ,(response/encode-url (make-anchor-url url anchor))) . ,rest)))
        ((record (@ (href-e ,url) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(record (@ (href ,(response/encode-url (make-anchor-url url anchor))) . ,rest) ,x ...))


        ;; @href-c [@bindings]
        ((a (@ (href-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(a (@ (href ,(make-context-url url bindings anchor)) . ,rest) ,x ...))
        ((link (@ (href-c ,url) (bindings (,bindings ())) . ,rest) ,[x] ...)
         (guard (string? url))
         `(link (@ (href ,(make-context-url url bindings)) . ,rest) ,x ...))
        ((form (@ (action-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(form (@ (action ,(make-context-url url bindings anchor)) . ,rest) ,x ...))
        ((img (@ (src-c ,url) (bindings (,bindings ())) . ,rest))
         (guard (string? url))
         `(img (@ (src ,(make-context-url url bindings)) . ,rest)))
        ((script (@ (src-c ,url) (bindings (,bindings ())) . ,rest) ,[x] ...)
         (guard (string? url))
         `(script (@ (src ,(make-context-url url bindings)) . ,rest) ,x ...))
        ((object (@ (data-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(object (@ (data ,(make-context-url url bindings anchor)) . ,rest) ,x ...))
        ((iframe (@ (src-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(iframe (@ (src ,(make-context-url url bindings anchor)) . ,rest) ,x ...))
        ((node (@ (href-c ,url) (bindings (,bindings ()))  (anchor (,anchor #f)) . ,rest))
         (guard (string? url))
         `(node (@ (href ,(make-context-url url bindings anchor)) . ,rest)))
        ((edge (@ (href-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest))
         (guard (string? url))
         `(edge (@ (href ,(make-context-url url bindings anchor)) . ,rest)))
        ((record (@ (href-c ,url) (bindings (,bindings ())) (anchor (,anchor #f)) . ,rest) ,[x] ...)
         (guard (string? url))
         `(record (@ (href ,(make-context-url url bindings anchor)) . ,rest) ,x ...))


        ;; @type=graphviz [@layout] [@format] [@alt-format]
        ((object (@ (type "graphviz") (layout (,layout "dot")) (format (,fmt "svg")) (alt-format (,alt-fmt #f)) . ,obj-attrs)
          (graph (@ (id ,id) . ,attrs) ,[x] ...))
         (make-graphviz layout id fmt obj-attrs `(graph (@ (id ,id) . ,attrs) ,x ...)))


        ;; everything that looks like an element, recur over
        (,elt (guard (xml-element? elt))
              (cons (xml-element-tag elt)
                    (let ((attributes (xml-element-attributes elt))
                          (contents (map T (xml-element-contents elt))))
                      (if (not (null? attributes))
                          (cons (cons '|@| attributes) contents)
                          contents))))


        ;; strings and symbols are untouched
        (,s (guard (or (string? s) (symbol? s))) s)


        ;; #f is converted to the empty string
        (,false (guard (not false)) "")


        ;; anything else is pretty-printed
        (,any (with-output-to-string (lambda () (pretty-print any))))))

    (T html))
    )