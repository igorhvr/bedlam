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

;;; This code is a derivative of the DOTML stylesheet developed by
;;; Martin Loetzsch (http://www.martin-loetzsch.de/DOTML).


(require-library 'sisc/libs/srfi/srfi-1) ; list library

(require-library 'sxml/sxml-match)
(require-library 'util/misc)


(module sxml/dotml
  (dotml->dot)

  (import string-io)
  (import type-system)

  (import srfi-1)

  (import sxml-matcher)
  (import util/misc)


  (define graph-attributes '(bgcolor fontcolor fontname fontsize label margin nodesep rankdir ranksep ratio size))
  (define cluster-attributes '(bgcolor color fillcolor fontcolor fontname fontsize label labeljust labelloc style))
  (define node-attributes '(color fillcolor fixedsize fontcolor fontname fontsize height href shape style URL width))
  (define record-attributes '(color fillcolor fontcolor fontname fontsize height href style URL width))
  (define edge-attributes '(arrowhead arrowsize arrowtail constraint color decorate dir fontcolor fontname fontsize headlabel headport href label labeldistance labelfloat labelfontcolor labelfontname labelfontsize minlen samehead sametail style taillabel tailport URL))


  (define (dotml->dot dotml)
    (sxml-match dotml
     ((graph (@ (id ,id) . ,attrs) ,[x] ...)
      (string-append "digraph "
                     id
                     " {compound=\"true\";"
                     (alist->attrs attrs ";")
                     x ...
                     "} "))
     ((sub-graph (@ (id ,id) (rank ,rank) . ,attrs) ,[x] ...)
      (string-append "subgraph sub_graph_"
                     id
                     " {"
                     (alist->attrs attrs ";")
                     x ...
                     "} "))
     ((cluster (@ (id ,id) . ,attrs) ,[x] ...)
      (string-append "subgraph cluster_"
                     id
                     " {"
                     (alist->attrs attrs ";")
                     x ...
                     "} "))

     ((node (@ (id ,id) . ,attrs))
      (format "~a[~a];" id (alist->attrs attrs ", ")))
     ((edge (@ (from ,src) (to ,dst) . ,attrs))
      (format "edge[~a] ~a -> ~a;" (alist->attrs attrs ", ") src dst))
     ((record (@ (id ,id) . ,attrs) ,x ...)
      (format "~a[shape=\"record\", ~alabel=\"~a\"];"
              id (alist->attrs attrs ", ")
              (make-record-label (cons 'record (list x ...)))))
     (,any (error
            (string-append
             "dotml->dot: Could not recognize element : \""
             (with-output-to-string (lambda () (pretty-print any)))
             "\"")))))

  (define (alist->attrs alist separator)
    (fold-right
     (lambda (pair str)
       (format "~a=\"~a\"~a~a" (car pair) (cadr pair) separator str))
     " "
     alist))

  (define (make-record-label record-elt)
    (sxml-match record-elt
      ((record ,[x] ...)
       (format  "{ ~s } "
                (fold-right (lambda (a b)
                              (if b (format "~a | ~a" a b) a))
                            #f (list x ...))))
      ((node (@ (label ,label)))
       label)
      ((node (@ (id ,id)))
       id)))
  )
