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
;;;   LShift Ltd. <query@lshift.net>
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

(require-library 'sql/jdbc)
(require-library 'sql/types)
(require-library 'sql/type-conversion)


(module sql/mssql
  ()

  (import s2j)
  (import hashtable)
  (import type-system)

  (import srfi-19)

  (import sql/jdbc)
  (import sql/types)
  (import sql/type-conversion)

  (define-java-classes
    (<object-input-stream> |java.io.ObjectInputStream|)
    (<object-output-stream> |java.io.ObjectOutputStream|))

  (define-generic-java-methods
    get-column-type-name
    get-nanos
    get-scale get-time
    get-binary-stream
    get-binary-output-stream
    read-object
    write-object
    (jclose |close|)
    (jflush |flush|)
    (jlength |length|)
    (jequals equals))

  (define (sql->time obj rsmd ji)
    (let ((ms (->number (get-time obj))))
      (make-time 'time-monotonic 0 (quotient ms 1000))))

  (define (sql->date obj rsmd ji)
    (let ((ms (->number (get-time obj))))
      (make-time 'time-monotonic 0 (quotient ms 1000))))

  (define (sql->timestamp obj rsmd ji)
    (let ((ms (->number (get-time obj)))
	  (ns (->number (get-nanos obj))))
      (make-time 'time-monotonic 0 (quotient ms 1000))))

  (define (sql->null obj rsmd ji)
    '())

  (define (sql->number obj rsmd ji)
    (if (= 0 (->number (get-scale rsmd ji)))
	(inexact->exact (->number obj))
	(->number obj)))

  (define (sql->object obj rsmd ji)
    (if (instance-of? obj <value>)
	(java-unwrap obj)
	obj))

  ;; GUIDs (uniqueidentifier) are given as the JDBC type VARCHAR and so appear
  ;; as strings, but they are unusable as SQL values unless wrapped in {}
  (define (guid-column? metadata jindex)
    (->boolean
     (jequals (->jstring "uniqueidentifier") (get-column-type-name metadata jindex))))

  (define (sql->string obj rsmd ji)
    (if (guid-column? rsmd ji)
	(string-append "{" (->string obj) "}")
	(->string obj)))

  (define (sql->boolean obj rsmid ji)
    (->boolean obj))

  (define (unimplemented obj rsmd ji)
    (->string obj)
    ;; (error (string-append "unimplemented type conversion from "
    ;;                      (->string (get-column-type-name rsmd ji))))
    )

  (add-sql->scheme-table
   '|microsoft sql server| ;
     `((,<sql-array> . ,unimplemented)
       (,<sql-bigint> . ,sql->number)
       (,<sql-binary> . ,unimplemented)
       (,<sql-bit> . ,sql->boolean)
       (,<sql-blob> . ,unimplemented)
       (,<sql-boolean> . ,sql->boolean)
       (,<sql-char> . ,sql->string)
       (,<sql-clob> . ,unimplemented)
       (,<sql-datalink> . ,unimplemented)
       (,<sql-date> . ,sql->date)
       (,<sql-decimal> . ,sql->number)
       (,<sql-distinct> . ,unimplemented)
       (,<sql-double> . ,sql->number)
       (,<sql-float> . ,sql->number)
       (,<sql-integer> . ,sql->number)
       (,<sql-java-object> . ,sql->object)
       (,<sql-longvarbinary> . ,unimplemented)
       (,<sql-longvarchar> . ,unimplemented)
       (,<sql-null> . ,sql->null)
       (,<sql-numeric> . ,sql->number)
       (,<sql-other> . ,sql->object)
       (,<sql-real> . ,sql->number)
       (,<sql-ref> . ,unimplemented)
       (,<sql-smallint> . ,sql->number)
       (,<sql-struct> . ,unimplemented)
       (,<sql-time> . ,sql->time)
       (,<sql-timestamp> . ,sql->timestamp)
       (,<sql-tinyint> . ,sql->number)
       (,<sql-varbinary> . ,unimplemented)
       (,<sql-varchar> . ,sql->string)))

  (jdbc/load-driver "net.sourceforge.jtds.jdbc.Driver")

  )
