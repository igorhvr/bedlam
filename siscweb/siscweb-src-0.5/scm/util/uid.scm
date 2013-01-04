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

(require-library 'sisc/libs/srfi/srfi-27) ; sources of random bits
(require-library 'sisc/libs/srfi/srfi-42) ; eager comprehensions

(module util/uid
  (uid/make-integer
   uid/make-rfc-1738-string)

  (import srfi-27)
  (import srfi-42)

  ;; by default SISC uses a cryptographically secure random source
  (define rand (random-source-make-integers default-random-source))

  (define (uid/make-integer)
    (rand 2147483647))

  (define (uid/make-rfc-1738-string)
    (let ((s (make-string uid-string-size)))
      (do-ec (:range i 0 uid-string-size)
             (:let n (rand (vector-length rfc-1738-chars)))
        (string-set! s i (vector-ref rfc-1738-chars n)))
      s))

  (define rfc-1738-chars
    (list->vector
     (append (list-ec (:char-range c #\0 #\9) c)
             (list-ec (:char-range c #\a #\z) c)
             (list-ec (:char-range c #\A #\Z) c)
             #|'(#\$ #\- #\_ #\. #\+ #\! #\* #\' #\( #\) #\,)|#))) ; elided for readability

  ;; we want the uid string to be equivalent to ~160 bits
  (define uid-string-size (inexact->exact (ceiling (/ (log (expt 2 160)) (log 62)))))

  )
