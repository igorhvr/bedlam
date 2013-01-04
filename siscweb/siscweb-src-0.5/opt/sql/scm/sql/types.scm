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


(module sql/types
  (<sql-array> <sql-bigint> <sql-binary> <sql-bit>
   <sql-blob> <sql-boolean> <sql-char> <sql-clob>
   <sql-datalink> <sql-date> <sql-decimal> <sql-distinct>
   <sql-double> <sql-float> <sql-integer> <sql-java-object>
   <sql-longvarbinary> <sql-longvarchar> <sql-null> <sql-numeric>
   <sql-other> <sql-real> <sql-ref> <sql-smallint>
   <sql-struct> <sql-time> <sql-timestamp> <sql-tinyint>
   <sql-varbinary> <sql-varchar>)

  (import s2j)

  (define-java-classes
    (<sql-types> |java.sql.Types|))


  (define (static class name)
    ((generic-java-field-accessor name) (java-null class)))

  (define <sql-array>)
  (define <sql-bigint>)
  (define <sql-binary>)
  (define <sql-bit>)
  (define <sql-blob>)
  (define <sql-boolean>)
  (define <sql-char>)
  (define <sql-clob>)
  (define <sql-datalink>)
  (define <sql-date>)
  (define <sql-decimal>)
  (define <sql-distinct>)
  (define <sql-double>)
  (define <sql-float>)
  (define <sql-integer>)
  (define <sql-java-object>)
  (define <sql-longvarbinary>)
  (define <sql-longvarchar>)
  (define <sql-null>)
  (define <sql-numeric>)
  (define <sql-other>)
  (define <sql-real>)
  (define <sql-ref>)
  (define <sql-smallint>)
  (define <sql-struct>)
  (define <sql-time>)
  (define <sql-timestamp>)
  (define <sql-tinyint>)
  (define <sql-varbinary>)
  (define <sql-varchar>)

  (set! <sql-array> (static <sql-types> '|ARRAY|))
  (set! <sql-bigint> (static <sql-types> '|BIGINT|))
  (set! <sql-binary> (static <sql-types> '|BINARY|))
  (set! <sql-bit> (static <sql-types> '|BIT|))
  (set! <sql-blob> (static <sql-types> '|BLOB|))
  (set! <sql-boolean> (static <sql-types> '|BOOLEAN|))
  (set! <sql-char> (static <sql-types> '|CHAR|))
  (set! <sql-clob> (static <sql-types> '|CLOB|))
  (set! <sql-datalink> (static <sql-types> '|DATALINK|))
  (set! <sql-date> (static <sql-types> '|DATE|))
  (set! <sql-decimal> (static <sql-types> '|DECIMAL|))
  (set! <sql-distinct> (static <sql-types> '|DISTINCT|))
  (set! <sql-double> (static <sql-types> '|DOUBLE|))
  (set! <sql-float> (static <sql-types> '|FLOAT|))
  (set! <sql-integer> (static <sql-types> '|INTEGER|))
  (set! <sql-java-object> (static <sql-types> '|JAVA_OBJECT|))
  (set! <sql-longvarbinary> (static <sql-types> '|LONGVARBINARY|))
  (set! <sql-longvarchar> (static <sql-types> '|LONGVARCHAR|))
  (set! <sql-null> (static <sql-types> '|NULL|))
  (set! <sql-numeric> (static <sql-types> '|NUMERIC|))
  (set! <sql-other> (static <sql-types> '|OTHER|))
  (set! <sql-real> (static <sql-types> '|REAL|))
  (set! <sql-ref> (static <sql-types> '|REF|))
  (set! <sql-smallint> (static <sql-types> '|SMALLINT|))
  (set! <sql-struct> (static <sql-types> '|STRUCT|))
  (set! <sql-time> (static <sql-types> '|TIME|))
  (set! <sql-timestamp> (static <sql-types> '|TIMESTAMP|))
  (set! <sql-tinyint> (static <sql-types> '|TINYINT|))
  (set! <sql-varbinary> (static <sql-types> '|VARBINARY|))
  (set! <sql-varchar> (static <sql-types> '|VARCHAR|))
  )
