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
;;; Ben Simon
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

(require-library 'util/misc)

(require-library 'sql/jdbc)
(require-library 'sql/types)
(require-library 'sql/type-conversion)


(module sql/postgresql
  ()

  (import s2j)
  (import hashtable)
  (import java-io)
  (import oo)
  (import serial-io)
  (import type-system)

  (import srfi-19)

  (import util/misc)

  (import sql/jdbc)
  (import sql/types)
  (import sql/type-conversion)


  (define-java-classes
    (<big-decimal> |java.math.BigDecimal|)
    (<big-integer> |java.math.BigInteger|)
    (<byte-array-input-stream> |java.io.ByteArrayInputStream|)
    (<byte-array-output-stream> |java.io.ByteArrayOutputStream|)
    (<character> |java.lang.Character|)
    (<date> |java.sql.Date|)
    (<double> |java.lang.Double|)
    (<integer> |java.lang.Integer|)
    (<long> |java.lang.Long|)
    (<jcharacter> |java.lang.Character|)
    (<jobject> |java.lang.Object|)
    (<time> |java.sql.Time|)
    (<timestamp> |java.sql.Timestamp|))

  (define-generic-java-methods
    get-column-type-name get-nanos get-scale get-time
    to-byte-array (jflush |flush|))


  (define (any->jbyte-array any)
    (let* ((jbyte-array (java-new <byte-array-output-stream>)))
      (call-with-serial-output-port (->binary-output-port jbyte-array)
        (lambda (port)
          (serialize any port)))
      (jflush jbyte-array)
      (to-byte-array jbyte-array)))


  (define (jbyte-array->any bytes)
    (let* ((jbyte-array (java-new <byte-array-input-stream> bytes)))
      (call-with-serial-input-port (->binary-input-port jbyte-array)
        (lambda (port)
          (deserialize port)))))


  (define VENDOR '|postgresql|)

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
        (jbyte-array->any obj)
        obj))

  (define (sql->string obj rsmd ji)
    (->string obj))

  (define (sql->boolean obj rsmid ji)
    (->boolean obj))

  (define (unimplemented obj rsmd ji)
    (error (string-append "unimplemented type conversion from "
                          (->string (get-column-type-name rsmd ji)))))


  (add-sql->scheme-table
   VENDOR
   `((,<sql-array> . ,unimplemented)
     (,<sql-bigint> . ,sql->number)
     (,<sql-binary> . ,sql->object)
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
     (,<sql-java-object> . ,unimplemented)
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

  (add-scheme->sql-proc
   VENDOR
   (lambda (value)
     (cond ((null? value) (java-null <jobject>))
           ((boolean? value) (->jbool value))
           ((or (string? value)
                (symbol? value))
            (->jstring value))
           ((char? value) (java-new <jcharacter> (->jchar value)))
           ;; vectors, pairs and procs are just wrapped
           ((vector? value) (any->jbyte-array value))
           ((procedure? value) (any->jbyte-array value))
           ((pair? value) (any->jbyte-array value))
           ((and (number? value)
                 (integer? value))
            (java-new <long> (->jstring (number->string (inexact->exact value)))))
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


  (jdbc/load-driver "org.postgresql.Driver")
  )
