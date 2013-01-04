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

(require-library 'sisc/libs/srfi/srfi-1) ; list library
(require-library 'sisc/libs/srfi/srfi-13) ; string library

(require-library 'siscweb/config)
(require-library 'sxml/sxml-match)
(require-library 'util/misc)
(require-library 'util/regexp)


(module sxml/markup
  (sxml->string)

  (import s2j)
  (import string-io)
  (import type-system)

  (import srfi-1)
  (import srfi-13)

  (import siscweb/config)
  (import sxml-matcher)
  (import util/misc)
  (import util/regexp)


  (define (sxml->string sxml)
    (define (xml-element? elt)
      (and (pair? elt)
           (symbol? (car elt))))

    (define (escape-string str)
      (fold-right (lambda (regexp replacement text)
                    (regexp-replace regexp text replacement))
                  str '("\"" "<" ">" "'" "&") '("&#34;" "&lt;" "&gt;" "&#39;" "&amp;")))

    (define (leaf? elt)
      (if (memq elt '(base br hr img input link meta)) #t #f))

    (define (->html-text value)
      (typecase value
        ((<string>) (escape-string value))
        ((<number>) (number->string value))
        (else (escape-string
               (with-output-to-string
                 (lambda () (pretty-print value)))))))

    (define (attr pair)
      (if (cadr pair) ; if attr value is #f, skip the attr
          (format " ~a=~s" (car pair) (->html-text (cadr pair)))
          ""))

    (import sxml-accessors)

    (sxml-match sxml
      ((*TOP* ,[x] ...)
       (string-append x ...))
      ((*DTD-INFO* ,name ,system)
       (guard (and (string? name) (string? system)))
       (format "<!DOCTYPE ~a SYSTEM ~s>" name system))
      ((*DTD-INFO/PUBLIC* ,name ,system ,public)
       (guard (and (string? name) (string? system) (string? public)))
       (format "<!DOCTYPE ~a PUBLIC ~s ~s>" name system public))
      ((*VERBATIM* ,string) (guard (string? string)) string)
      ((*COMMENT* ,string)
       (guard (and (string? string)
                   (not (string-contains string "--"))))
       (if (config/sxml-output-comments?)
           (format "<!--~a-->" string)
           ""))
      ((& ,symbol)
       (guard (symbol? symbol))
       (format "&~a;" symbol))
      (,elt
       (guard (and (xml-element? elt)
                   (or (leaf? (car elt))
                       (null? (xml-element-contents elt)))))
       (format "<~a~a />"
               (car elt)
               (fold-right string-append ""
                           (map attr
                                (xml-element-attributes elt)))))
      (,elt
       (guard (xml-element? elt))
       (format "<~a~a>~a</~a>"
               (car elt)
               (fold-right string-append ""
                           (map attr
                                (xml-element-attributes elt)))
               ;; tag content, properbly terminated
               (let ((c (xml-element-contents elt)))
                 (fold-right string-append "" (map sxml->string c)))
               (car elt)))
      (,string (guard (string? string)) (escape-string string))
      (,number (guard (number? number)) (number->string number))
      (,symbol (guard (symbol? symbol))
               (format "&~a;" symbol))
      (#f "")
      (,else (error (string-append
                     "siscweb/html: Invalid element : \""
                     (with-output-to-string
                       (lambda ()
                         (display else)))
                     "\"")))))
  )
