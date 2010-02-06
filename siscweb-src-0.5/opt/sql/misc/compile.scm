(require-library 'sisc/libs/srfi)
(require-library 'util/misc)

(require-library 'lshift/common)
(require-library 'lshift/error)
(require-library 'lshift/s2j-modutils)

(import file-manipulation)

(define dest "../bin/")

(current-directory "../scm")

(for-each
 (lambda (name)
   (make-directory! (string-append dest name)))
 '("sql" "siscweb"))

(for-each
 (lambda (name)
   (display (string-append "Compiling " name " ..."))
   (compile-file (string-append name ".scm")
                 (string-append dest name ".scc"))
   (display "done.\n"))
 '("sql/jdbc" "sql/types" "sql/type-conversion" "sql/result-set" "sql/query"
   "sql/hsqldb" "sql/mssql" "sql/oracle" "sql/postgresql"
   "sql/types" "sql/type-conversion"))
