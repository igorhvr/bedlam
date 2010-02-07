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


(require-library 'sisc/libs/srfi/srfi-42) ; eager comprehensions
(require-library 'sisc/libs/srfi/srfi-45) ; primitives for expressing iterative lazy algorithms

(require-library 'sql/jdbc)
(require-library 'sql/result-set)
(require-library 'sql/type-conversion)


(module sql/query
  (sql/execute sql/execute-query sql/execute-update
   sql/map-row sql/for-each-row)

  (import s2j)
  (import generic-procedures)
  (import hashtable)

  (import sql/jdbc)
  (import sql/result-set)
  (import sql/type-conversion)

  (import srfi-42)
  (import srfi-45)

  (define-generic-java-methods
    close
    get-connection get-column-name get-result-set
    get-update-count get-meta-data
    execute execute-query execute-update next
    prepare-statement set-null set-object)


  (define (sql/execute conn sql . bindings)
    (let ((pstmt (prepare-statement conn (->jstring sql))))
      (with/fc
       (lambda (e m)
         (close pstmt)
         (throw e))
       (lambda ()
         (bind-statement! pstmt bindings)
         (let ((rs? (execute pstmt)))
           (cond ((->boolean rs?)
                  (make-cursor pstmt (jdbc/get-vendor conn)))
                 (else
                  (dynamic-wind
                      (lambda () #t)
                      (lambda ()
                        (->number (get-update-count pstmt)))
                      (lambda ()
                        (close pstmt))))))))))

  (define (sql/execute-query conn sql . bindings)
    (let ((pstmt (prepare-statement conn (->jstring sql))))
      (with/fc
       (lambda (e m)
         (close pstmt)
         (throw e))
       (lambda ()
         (bind-statement! pstmt bindings)
         (execute-query pstmt)
         (make-cursor pstmt (jdbc/get-vendor conn))))))


  (define (sql/execute-update conn sql . bindings)
    (let ((pstmt (prepare-statement conn (->jstring sql))))
      (dynamic-wind
          (lambda () #t)
          (lambda ()
            (bind-statement! pstmt bindings)
            (let ((vendor (jdbc/get-vendor conn)))
              (->number (execute-update pstmt))))
          (lambda ()
            (close pstmt)))))


  (define-syntax sql/map-row
    (syntax-rules ()
      ((_ resultset ((var name) ...) body ...)
       (rs/map
        (lambda (ht)
          (let ((var (hashtable/get ht name)) ...)
            body ...))
        resultset))))


  (define-syntax sql/for-each-row
    (syntax-rules ()
      ((_ resultset ((var name) ...) body ...)
       (rs/for-each
        (lambda (ht)
          (let ((var (hashtable/get ht name)) ...)
            body ...))
        resultset))))


  (define (bind-statement! pstmt bindings)
    (let* ((vendor (jdbc/get-vendor (get-connection pstmt)))
           (scheme->sql (get-scheme->sql-proc vendor)))
      (if scheme->sql
          (do-ec (:parallel (:range i 1 (+ 1 (length bindings)))
                            (:list value bindings))
                 (set-object pstmt (->jint i) (scheme->sql value)))
          (error (format "Unknown vendor \"~a\". Did you forget to import the corresponding module?" vendor)))))

  (define (prepare-and-bind-statement conn sql bindings)
    (let ((pstmt (prepare-statement conn (->jstring sql)))
          (scheme->sql (get-scheme->sql-proc (jdbc/get-vendor conn))))
      (do-ec (:parallel (:range i 1 (+ 1 (length bindings)))
                        (:list value bindings))
        (set-object pstmt (->jint i) (scheme->sql value)))
      pstmt))

  (define (make-cursor pstmt vendor)
    (let* ((rs (get-result-set pstmt))
           (:rs (make-rs-accessor rs vendor))
           (get-row (lambda ()
                      (let ((row (make-hashtable string=? #f)))
                        (do-ec (:list name (get-column-names rs))
                          (hashtable/put! row name (:rs name)))
                        row)))
           (cleanup (lambda ()
                     (dynamic-wind
                         (lambda () #t)
                         (lambda () (close rs))
                         (lambda () (close pstmt))))))
      (letrec ((L
        (lambda ()
          (with/fc
           (lambda (e m)
             (cleanup)
             (throw e))
           (lambda ()
             (lazy
              (cond ((not (->boolean (next rs)))
                     (cleanup)
                     (delay '()))
                    (else
                     (delay (cons (get-row) (L)))))))))))
        (L))))

  )
