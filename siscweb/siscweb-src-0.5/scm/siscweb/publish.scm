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

(require-library 'sisc/libs/srfi/srfi-1) ; list library
(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity
(require-library 'sisc/libs/srfi/srfi-69) ; basic hash tables

(require-library 'util/misc)
(require-library 'util/regexp)


(module siscweb/publish
  (published? get-published get-all-published
   publish publish/regexp publish/wildcard publish-bulk
   unpublish unpublish-all)

  (import srfi-1)
  (import srfi-16)
  (import srfi-69)

  (import util/misc)
  (import util/regexp)


  (define (published? path-info)
    (and (get-published path-info) #t))


  (define (publish/regexp regexp-pattern obj)
    (publish regexp-pattern obj 'regexp))

  (define (publish/wildcard wildcard-pattern obj)
    (publish wildcard-pattern obj 'wildcard))

  (define publish
    (case-lambda
      ((path-pattern obj)
       (publish path-pattern obj 'wildcard))
      ((path-pattern obj notation)
       (let ((regexp-pattern (case notation
                               ((wildcard)
                                (wildcard->regexp path-pattern))
                               (else
                                path-pattern))))
         (hash-table-set! mappings
                          path-pattern
                          (list regexp-pattern
                                obj
                                notation))))))

  (define (unpublish path-pattern)
    (hash-table-delete! mappings path-pattern))

  ;; TODO: some memoization of the association between
  ;;       path-info -> pattern key might be good here;
  ;;       most path-info requests are going to be the same
  (define (get-published path-info)
    (define (find-published)
      (let/cc escape-k
        (hash-table-walk
         mappings
         (lambda (path-pattern regexp-obj-lst)
           (when (regexp-match (car regexp-obj-lst) path-info)
             (escape-k (cadr regexp-obj-lst)))))
        #f))

    (let ((obj (find-published)))
      (cond ((procedure? obj) obj)
            ((symbol? obj) (eval obj))
            (else #f))))


  (define (get-all-published)
    (hash-table-fold
     mappings
     (lambda (path-pattern regexp-obj-lst acc)
       (cons (cons path-pattern (cdr regexp-obj-lst)) acc))
     '()))

  (define (publish-bulk alist)
    (for-each
     (lambda (entry)
       (apply publish entry))
     alist))

  (define (unpublish-all)
    (set! mappings (make-hash-table string=?)))

  (define (wildcard->regexp wildcard-pattern)

    ;; the transforms to apply; order is important
    (define transforms
      (map (lambda (entry)
             (cons (make-regexp (car entry)) (cdr entry)))
             ;; 1. /a/* matches /a, /a/, /a/b, /a/b/c, etc.
           '(("/\\*$" . "(/\\\\p{Graph}*)*")
             ;; 2. /a/**/b matches /a/b, /a/c/b, /a/c/d/b, etc.
             ("/\\*\\*/" . "(/[\\\\p{Alnum}-_\\$\\+\\,\\.\\!\\~\\*\\'\\(\\)]+)*/")
             ;; 3. /a/*/b matches /a/c/b but not /a/b nor /a/c/d/b
             ("/\\*/" . "(/[\\\\p{Alnum}-_\\$\\+\\,\\.\\!\\~\\*\\'\\(\\)]+){1}/"))))

    (make-regexp
     (format "^~a$"
             (fold-right
              (lambda (transform pattern)
                (regexp-replace (car transform) pattern (cdr transform)))
              wildcard-pattern
              transforms))))

  (define mappings (make-hash-table string=?))

  )
