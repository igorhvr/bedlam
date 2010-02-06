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

(require-library 'sisc/libs/srfi/srfi-78) ; lightweight testing

(require-library 'siscweb/bindings)

(module test/bindings
  (test-bindings)

  (import srfi-78)

  (import siscweb/bindings)

  (define binding-alist `((a 1) (b "2") ("c" 3) ("d" "4")
                          (e . 5) (f . "6") ("g" . 7) ("h" . "8")
                          (i 1 2) (j "2" "3") ("k" 3 4) (l . ("6" "7")) ("m" . (7 8))
                          (x) (y . #f)))

  (define (test-bindings)
    (let ((bindings (alist->bindings binding-alist)))
      (check-reset!)

      ;; 1. checks extract-bindings
      (check-ec (:list x binding-alist)
                (:let key (if (symbol? (car x))
                              (car x)
                              (string->symbol (car x))))
                (:let expected-value (if (not (cdr x))
                                         '()
                                         (cdr x)))
                (:let actual-value (extract-bindings key bindings))
                actual-value => expected-value)

      ;; 2. checks extract-single-binding
      (check-ec (:list x binding-alist)
                (:let key (if (symbol? (car x))
                              (car x)
                              (string->symbol (car x))))
                (:let expected-value (cond ((null? (cdr x)) #f)
                                           ((pair? (cdr x)) (cadr x))
                                           (else (cdr x))))
                (:let actual-value (extract-single-binding key bindings))
                actual-value => expected-value)

      ;; checks exists-binding? (1/2)
      (check-ec (:list x binding-alist)
                (:let key (car x))
                (:let flip-key (if (symbol? (car x))
                                   (symbol->string (car x))
                                   (string->symbol (car x))))
                (and (exists-binding? key bindings)
                     (exists-binding? flip-key bindings))
                => #t)

      ;; check exists-binding (1/2)
      (check (exists-binding? 'xxx bindings) => #f)

      ;; final report
      (check-report)))
  )
