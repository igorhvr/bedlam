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


(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures

(module sql/type-conversion
  (get-sql->scheme-proc add-sql->scheme-table
   get-scheme->sql-proc add-scheme->sql-proc)

  (import hashtable)

  (define sql->scheme-tables
    (make-hashtable eq? #f))

  (define (add-sql->scheme-table vendor alist)
    (hashtable/put! sql->scheme-tables
                    vendor
                    (alist->hashtable alist eqv? #f)))


  (define (get-sql->scheme-proc sql-type vendor)
    (hashtable/get (hashtable/get sql->scheme-tables vendor) sql-type))


  (define scheme->sql-procs
    (make-hashtable eq? #f))

  (define (add-scheme->sql-proc vendor proc)
    (hashtable/put! scheme->sql-procs vendor proc
                    (lambda (x)
                      (error (string-append "sql/type-conversion : unknown vendor "
                                            (symbol->string vendor))))))

  (define (get-scheme->sql-proc vendor)
    (hashtable/get scheme->sql-procs vendor))
)
