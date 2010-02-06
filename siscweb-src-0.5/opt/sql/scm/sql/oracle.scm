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


(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures

(require-library 'sql/jdbc)
(require-library 'sql/types)
(require-library 'sql/type-conversion)

(require-library 'util/misc)


(module sql/oracle
  (oracle/read-blob oracle/write-blob)

  (import s2j)
  (import hashtable)
  (import type-system)

  (import srfi-19)

  (import util/misc)

  (import sql/jdbc)
  (import sql/types)
  (import sql/type-conversion)


  (define-java-classes
    (<big-decimal> |java.math.BigDecimal|)
    (<big-integer> |java.math.BigInteger|)
    (<date> |java.sql.Date|)
    (<double> |java.lang.Double|)
    (<integer> |java.lang.Integer|)
    (<object> |java.lang.Object|)
    (<object-input-stream> |java.io.ObjectInputStream|)
    (<object-output-stream> |java.io.ObjectOutputStream|)
    (<time> |java.sql.Time|)
    (<timestamp> |java.sql.Timestamp|))

  (define-generic-java-methods get-column-type-name get-nanos get-scale get-time
    get-binary-stream get-binary-output-stream read-object write-object (jclose |close|) (jflush |flush|) (jlength |length|))


  (define VENDOR '|oracle|)

  (define (oracle/read-blob blob)
    (let* ((bis (get-binary-stream blob))
           (ois (java-new <object-input-stream> bis)))
    (read-object ois)))

  (define (oracle/write-blob blob jobj)
    (let* ((bos (get-binary-output-stream blob))
           (oos (java-new <object-output-stream> bos)))
      (write-object oos jobj)
      (jflush oos)
      (jflush bos)
      (->number (jlength blob))))


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

  (define (sql->string obj rsmd ji)
    (->string obj))


  (define (unimplemented-sql obj rsmd ji)
    (error (string-append "sql/oracle: unimplemented type conversion from "
                          (->string (get-column-type-name rsmd ji)))))

  (define (unimplemented-scheme name)
    (error (string-append "sql/oracle: unimplemented type conversion from "
                          name)))

  (add-sql->scheme-table
   VENDOR
   `((,<sql-array> . ,unimplemented-sql)
     (,<sql-bigint> . ,sql->number)
     (,<sql-binary> . ,unimplemented-sql)
     (,<sql-bit> . ,unimplemented-sql)
     (,<sql-blob> . ,(lambda (x y z) x))
     (,<sql-boolean> . ,unimplemented-sql)
     (,<sql-char> . ,sql->string)
     (,<sql-clob> . ,unimplemented-sql)
     (,<sql-datalink> . ,unimplemented-sql)
     (,<sql-date> . ,sql->date)
     (,<sql-decimal> . ,sql->number)
     (,<sql-distinct> . ,unimplemented-sql)
     (,<sql-double> . ,sql->number)
     (,<sql-float> . ,sql->number)
     (,<sql-integer> . ,sql->number)
     (,<sql-java-object> . ,sql->object)
     (,<sql-longvarbinary> . ,unimplemented-sql)
     (,<sql-longvarchar> . ,unimplemented-sql)
     (,<sql-null> . ,sql->null)
     (,<sql-numeric> . ,sql->number)
     (,<sql-other> . ,sql->object)
     (,<sql-real> . ,sql->number)
     (,<sql-ref> . ,unimplemented-sql)
     (,<sql-smallint> . ,sql->number)
     (,<sql-struct> . ,unimplemented-sql)
     (,<sql-time> . ,sql->time)
     (,<sql-timestamp> . ,sql->timestamp)
     (,<sql-tinyint> . ,sql->number)
     (,<sql-varbinary> . ,unimplemented-sql)
     (,<sql-varchar> . ,sql->string)))

  (add-scheme->sql-proc
   VENDOR
   (lambda (value)
     (cond ((null? value) (java-null <object>))
           ((boolean? value) (->jbool value))
           ((or (string? value)
                (symbol? value))
            (->jstring value))
           ((char? value) (->jchar value))
           ((vector? value) (unimplemented-scheme "vector"))
           ((procedure? value) (unimplemented-scheme "procedure"))
           ((pair? value) (unimplemented "pair"))
           ((and (number? value)
                 (integer? value))
            (java-new <integer> (->jstring (number->string value))))
           ((and (number? value)
                 (real? value))
            (java-new <big-decimal> (->jstring (number->string value))))
           ((and (number? value)
                 (rational? value))
            ((java-new <big-decimal> (->jstring (number->string (exact->inexact value))))))
           ((date? value)
            (let ((t (date->time-utc value)))
              (java-new <date> (->jlong (* 1000 (time-second t))))))
           ((time? value)
            (java-new <timestamp> (->jlong (* 1000 (time-second value)))))
           (else value))))

  (jdbc/load-driver "oracle.jdbc.OracleDriver")
  )
