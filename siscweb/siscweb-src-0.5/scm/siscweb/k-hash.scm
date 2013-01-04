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

(require-library 'sisc/libs/srfi/srfi-38) ; intermediate formatting
(require-library 'sisc/libs/srfi/srfi-48) ; intermediate format strings

(require-library 'util/regexp)
(require-library 'util/uid)

(module siscweb/k-hash
  (extract-k-hash make-k-url
   make-random-hash)

  (import srfi-38)
  (import srfi-48)

  (import util/regexp)
  (import util/uid)

  (define (extract-k-hash url)
    (and (regexp-match url-re url)
         (regexp-replace url-re url "$2")))

  (define (make-k-url request-uri k-hash)
    (if (regexp-match? url-re request-uri)
        (regexp-replace url-re request-uri (format "$1~a$3" k-hash))
        (format "~a;k-hash=~a" request-uri k-hash)))

  (define make-random-hash uid/make-rfc-1738-string)

  ;; the p{Alnum} part only works if uid/make-rfc-1738-string returns alphanums
  (define url-re (make-regexp "(\\A.*;k-hash=)(\\p{Alnum}+)([;/].*\\z|.*\\z)"))
  )
