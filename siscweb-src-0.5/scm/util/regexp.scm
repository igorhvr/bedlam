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


(module util/regexp
  (<regexp> make-regexp regexp->string
            regexp-match? regexp-match regexp-match-positions
            regexp-split regexp-replace)

  (import s2j)
  (import generic-procedures)
  (import type-system)

  (define-java-classes
    (<matcher> |java.util.regex.Matcher|)
    (<regexp> |java.util.regex.Pattern|)
    (<string-buffer> |java.lang.StringBuffer|))

  (define-generic-java-methods
    append-replacement append-tail compile matcher matches find start end replace-all split pattern)


  (define (make-regexp str)
    (compile (java-null <regexp>)
             (->jstring str)))

  (define-generic regexp->string)

  (define-method (regexp->string (<regexp> regexp))
    (->string (pattern regexp)))

  (define-generic regexp-match-positions)


  (define-method (regexp-match-positions (<regexp> regexp) (<string> text)
                                         (<number> s) (<number> e))
    (let* ((m (matcher regexp (->jstring text)))
           (match? (->boolean (find m (->jint s)))))
      (and match?
           (reverse
            (let loop ((match? match?)
                       (lst '()))
              (if (not match?)
                  lst
                  (let ((match-start (->number (start m)))
                        (match-end (->number (end m))))
                    (if (<= s match-start match-end e)
                        (loop (->boolean (find m))
                              (cons `(,match-start . ,match-end) lst))
                        lst))))))))

  (define-method (regexp-match-positions (<regexp> regexp) (<string> text))
    (regexp-match-positions regexp text 0 (string-length text)))

  (define-method (regexp-match-positions (<string> regexp) (<string> text))
    (regexp-match-positions (make-regexp regexp) text))

  (define-method (regexp-match-positions (<string> regexp) (<string> text)
                                         (<number> s) (<number> e))
    (regexp-match-positions (make-regexp regexp) text s e))


  (define-generic regexp-match?)

  (define-method (regexp-match? (<regexp> regexp) (<string> text))
    (let ((m (matcher regexp (->jstring text))))
      (->boolean (find m (->jint 0)))))

  (define-method (regexp-match? (<string> regexp) (<string> text))
    (regexp-match? (make-regexp regexp) text))


  (define-generic regexp-match)

  (define-method (regexp-match (<regexp> regexp) (<string> text)
                               (<number> s) (<number> e))
    (let* ((m (matcher regexp (->jstring text)))
           (match? (->boolean (find m (->jint s)))))
      (and match?
           (reverse
            (let loop ((match? match?)
                       (lst '()))
              (if (not match?)
                  lst
                  (let ((match-start (->number (start m)))
                        (match-end (->number (end m))))
                    (if (<= s match-start match-end e)
                        (loop (->boolean (find m))
                              (cons (substring text match-start match-end) lst))
                        lst))))))))


  (define-method (regexp-match (<regexp> regexp) (<string> text))
    (regexp-match regexp text 0 (string-length text)))

  (define-method (regexp-match (<string> regexp) (<string> text))
    (regexp-match (make-regexp regexp) text 0 (string-length text)))

  (define-method (regexp-match (<string> regexp) (<string> text)
                               (<number> s) (<number> e))
    (regexp-match (make-regexp regexp) text s e))



  (define-generic regexp-split)

  (define-method (regexp-split (<regexp> regexp) (<string> text))
    (map (lambda (str)
           (->string str))
         (->list (split regexp (->jstring text)))))

  (define-method (regexp-split (<string> regexp) (<string> text))
    (regexp-split (make-regexp regexp) text))


  (define-generic regexp-replace)

  (define-method (regexp-replace (<regexp> regexp) (<string> text) (<string> replacement))
    (->string (replace-all (matcher regexp (->jstring text))
                           (->jstring replacement))))

  (define-method (regexp-replace (<string> regexp) (<string> text) (<string> replacement))
    (regexp-replace (make-regexp regexp) text replacement))
  )
