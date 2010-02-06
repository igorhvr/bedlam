;;; Copyright (c) 2005, 2006 LShift Ltd. <query@lshift.net>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(require-library 'sisc/libs/srfi/srfi-42) ; list library
(require-library 'sisc/libs/srfi/srfi-13) ; string library
(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures
(require-library 'sisc/libs/srfi/srfi-26) ; notation for specialized parameters without currying
(require-library 'sisc/libs/srfi/srfi-45) ; primitives for expressing iterative lazy algorithms

(require-library 'sql/jdbc)
(require-library 'sql/query)
(require-library 'util/jndi)

(require-library 'lshift/common)
(require-library 'lshift/error)
(require-library 'lshift/sxml-tools)
(require-library 'lshift/s2j-modutils)

(module lshift/db
    (database-descriptor
     make-database-descriptor
     database-descriptor?
     database-descriptor-name
     database-descriptor-fresh-connection-procedure
     database-descriptor-current-connection
     database-descriptor-txn-cache
     database-descriptor-result-classes

     current-connection-to
     transaction-active?
     call-with-connection-to
     call-with-transaction-on
     call-with-simple-transaction-on

     (with-connection-to call-with-connection-to)
     (with-transaction-on call-with-transaction-on)
     (with-simple-transaction-on call-with-simple-transaction-on)

     db-field-type
     make-db-field-type
     db-field-type?
     db-field-type-name
     db-field-type-scheme->sxml
     db-field-type-sxml->scheme

     make-db-result-class
     db-result-class?
     db-result-class-name
     db-result-class-data
     db-result-class-fields
     db-result-class-pk-selector
     db-result-class-selector
     db-result-class-saver

     get-db-field-type
     db-result-class-print-string
     db-result-class-field-names
     db-result-class-field-type

     db-result
     make-db-result
     db-result?
     db-result-class
     db-result-data

     copy-db-result
     make-db-result0
     db-result-get
     db-result-set!
     db-result-diff
     db-result-grouping-fold
     db-result->alist

     db-result->sxml
     basic-join-specs
     db-result->sxml/join-specs
     sxml->db-result*
     sxml->db-result

     db-results->one-to-one
     db-results->one-to-many

     literal-sql
     make-literal-sql
     literal-sql?
     literal-sql-expr

     sql-expr
     make-sql-expr
     sql-expr?
     sql-expr-ast

     parse-date
     money->sxml
     empty-string-guard
     null-guard

     db-select-by-pk
     db-select
     db-select1
     db-save!

     make-new-guid-pk

     register-table-result-class!
     db-table-save-result-pk
     db-table-save-result-pk1

     register-view-result-class!

     sql-expr->string
     sql-result-cursor->list
     run-sql-expr-query
     run-sql-expr-statement

     (define-database make-database-descriptor make-hashtable)
     (define-database-view register-view-result-class!)
     (define-database-table register-table-result-class!)

     get-jndi-connection
     )

  (import srfi-1)
  (import srfi-13)
  (import srfi-19)
  (import srfi-26)
  (import srfi-45)

  (import sql/jdbc)
  (import sql/query)
  (import util/jndi)

  (import lshift/common)
  (import lshift/error)
  (import lshift/sxml-tools)

  (import string-io)
  (import hashtable)
  (import record) ;; includes SRFI-9 compatibility
  (import pattern-matching)

  (import s2j)
  (import lshift/s2j-modutils)

  (module-define-generic-java-methods
     set-auto-commit
     commit
     rollback)

  ;;===========================================================================
  ;; PART 1: Database Descriptor objects; Connection and Transaction Management
  ;;===========================================================================

  ;;---------------------------------------------------------------------------
  ;; The read/write code uses database-descriptor objects. The
  ;; database-descriptor object for a database definition is built by the
  ;; expansion of the |database| macro.
  ;;
  ;; All database access and update routines require an active
  ;; transaction. The main interfaces for getting active transactions are:
  ;;
  ;; - macro (with-simple-transaction-on dbdesc (rollback) body ...)
  ;;
  ;; Evaluates body with an active connection and transaction on
  ;; the database described by the given database-descriptor.
  ;; The "rollback" identifier is bound in the scope of the body
  ;; to a procedure that causes a database rollback and returns
  ;; its argument as the result of the whole with-simple-transaction-on
  ;; form.
  ;;
  ;; - macro (with-transaction-on dbdesc (rollback) body ...)
  ;;
  ;; As per with-simple-transaction-on, but the result returned
  ;; (either through normal flow-of-control, or through the rollback
  ;; procedure) must be a function of no arguments, that is then
  ;; called outside the transaction.
  ;;
  ;; It is an error to attempt to nest transactions. It is an error to
  ;; attempt to escape and reenter transactions - note in particular that a
  ;; transaction may not span a user interaction!
  ;;
  ;; Each transaction starts off with a fresh, empty db-result cache,
  ;; that can be filled and accessed using db-select-by-pk.

  (define-record-type database-descriptor
    (make-database-descriptor name ;; symbol
			      fresh-connection-procedure ;; unit -> connection
			      current-connection ;; parameter-of connection-or-false
			      txn-cache	;; parameter-of cache-or-false
			      result-classes ;; hashtable-of symbol -> db-result-class
			      )
    database-descriptor?
    (name database-descriptor-name)
    (fresh-connection-procedure database-descriptor-fresh-connection-procedure)
    (current-connection database-descriptor-current-connection)
    (txn-cache database-descriptor-txn-cache)
    (result-classes database-descriptor-result-classes))

  (define (dbdesc-error message dbdesc)
    (error+ message (database-descriptor-name dbdesc)))

  (define (current-connection-to dbdesc require-transaction)
    (and (or (not require-transaction)
	     ((database-descriptor-txn-cache dbdesc))
	     (dbdesc-error "No active transaction for database" dbdesc))
	 (or ((database-descriptor-current-connection dbdesc))
	     (dbdesc-error "No active connection to database" dbdesc))))

  (define (transaction-active? dbdesc)
    (not (not ((database-descriptor-txn-cache dbdesc)))))

  (define (register-result-class! dbdesc name result-class)
    (hashtable/put! (database-descriptor-result-classes dbdesc) name result-class)
    result-class)

  (define (lookup-result-class dbdesc name)
    (hashtable/get (database-descriptor-result-classes dbdesc) name #f))

  (define (make-fresh-db-result-cache)
    (make-hashtable equal?))

  (define (call-with-connection-to dbdesc fn)
    (let ((conn-param (database-descriptor-current-connection dbdesc)))
      (if (conn-param)
	  (fn)
	  (let* ((new-conn ((database-descriptor-fresh-connection-procedure dbdesc)))
		 (_ (set-auto-commit new-conn (->jboolean #f)))
		 (dead #f)
		 (finish (lambda ()
			   (if (not dead) (begin (set! dead #t)
						 (jdbc/close-connection new-conn))))))
	    (dynamic-wind
		(lambda ()
		  (if dead (dbdesc-error "Attempt to reenter dead database-session" dbdesc)))
		(lambda ()
		  (with/fc
		      (lambda (m e)
			(rollback new-conn)
			(finish)
			(throw m e))
		    (lambda ()
		      (parameterize ((conn-param new-conn))
			(fn)))))
		(lambda ()
		  (finish)))))))

  (define (call-with-transaction-on dbdesc fn)
    (let ((final-action
	   (call-with-current-continuation
	    (lambda (escape)
	      (call-with-connection-to dbdesc
		(lambda ()
		  (if (transaction-active? dbdesc)
		      (dbdesc-error "Attempt to nest transactions for database" dbdesc)
		      (let* ((dead #f)
			     (finish (lambda (action)
				       (if (not dead)
					   (begin (set! dead #t)
						  (action (current-connection-to dbdesc #f)))))))
			(dynamic-wind
			    (lambda ()
			      (if dead (dbdesc-error "Attempt to reenter dead transaction"
						     dbdesc)))
			    (lambda ()
			      (with/fc
				  (lambda (m e)
				    (finish rollback)
				    (throw m e))
				(lambda ()
				  (let* ((rollback-fn
					  (lambda (result)
					    (if dead
						(dbdesc-error
						 "Attempt to rollback dead transaction"
						 dbdesc))
					    (finish rollback)
					    (escape result)))
					 (result
					  (parameterize (((database-descriptor-txn-cache dbdesc)
							  (make-fresh-db-result-cache)))
					    (fn rollback-fn))))
				    (if dead
					(dbdesc-error "Attempt to commit dead transaction" dbdesc))
				    (finish commit)
				    result))))
			    (lambda ()
			      (if (not dead)
				  (begin
				    (finish rollback)
				    (dbdesc-error "Attempt to escape from live transaction"
						  dbdesc)))))))))))))
      (if (not (procedure? final-action))
	  (error+ "call-with-transaction-on requires procedure as final-action"
		  (database-descriptor-name dbdesc)
		  final-action))
      (final-action)))

  (define (call-with-simple-transaction-on dbdesc fn)
    (call-with-transaction-on dbdesc
			      (lambda (rollback-fn)
				(let ((result (fn (lambda (result)
						    (rollback-fn (lambda () result))))))
				  (lambda () result)))))

  (define-syntax with-connection-to
    (syntax-rules ()
      ((_ dbdesc body ...)
       (call-with-connection-to dbdesc
				(lambda ()
				  body ...)))))

  (define-syntax with-transaction-on
    (syntax-rules ()
      ((_ dbdesc (rollback-fn-name) body ...)
       (call-with-transaction-on dbdesc
				 (lambda (rollback-fn-name)
				   body ...)))))

  (define-syntax with-simple-transaction-on
    (syntax-rules ()
      ((_ dbdesc (rollback-fn-name) body ...)
       (call-with-simple-transaction-on dbdesc
					(lambda (rollback-fn-name)
					  body ...)))))

  ;;===========================================================================
  ;; PART 2: Table and View descriptions; SQL abstraction; SXML conversion
  ;;===========================================================================

  ;; All type definitions up front, so that procedures referring to these
  ;; types get the correct type instance.

  (define-struct db-field-type (name scheme->sxml sxml->scheme))

  ;; name :: symbol.
  ;; fields :: hashtable mapping name symbol to db-field-type
  ;; pk-selector :: class * (list-of pk-values) -> db-result or false
  ;; selector :: class * predicates * parameters -> list-of db-result
  ;; saver :: class * db-result * is-explicit-insert -> unspecified
  ;;
  (define-record-type <db-result-class>
    (make-db-result-class database-descriptor name data fields pk-selector selector saver)
    db-result-class?
    (database-descriptor db-result-class-database-descriptor)
    (name db-result-class-name)
    (data db-result-class-data)
    (fields db-result-class-fields)
    (pk-selector db-result-class-pk-selector)
    (selector db-result-class-selector)
    (saver db-result-class-saver))

  (define-record-type db-result
    (make-db-result* class data)
    db-result?
    (class db-result-class)
    (data db-result-data))

  (define-struct db-table-result-class-data (name table-name oid-fields auto-pk-fn field-specs))

  (define-struct db-table-result-class-field-spec
    (name
     type
     default-value))

  (define-struct literal-sql (expr))

  (define-struct sql-expr (ast))

  ;;---------------------------------------------------------------------------
  ;; Now all the code, *after* the type definitions.

  ;; Tries to parse a date out of a string.
  (define parse-date
    (let ((formats '("~d/~m/~Y"	;; dd/mm/yyyy
		     "~d-~m-~Y"	;; dd/mm/yyyy
		     "~Y/~m/~d"	;; yyyy/mm/dd
		     "~Y-~m-~d"	;; yyyy-mm-dd
		     "~d ~b ~Y"	;; dd Mon yyyy
		     "~d ~B ~Y"	;; dd Month yyyy
		     "~Y ~b ~d"	;; yyyy Mon dd
		     "~Y ~B ~d"))) ;; yyyy Month dd
      (define (fixup-year y)
	(cond
	 ((< y 50) (+ y 2000))
	 ((< y 100) (+ y 1900))
	 (else y)))
      (lambda (str0)
	(and (string? str0)
	     (let* ((str (string-titlecase str0)) ;; eww, ~b is case sensitive for titlecase!!
		    (date (call/ec (lambda (return)
				     (for-each (lambda (format)
						 (with/fc (lambda _ #f)
						   (lambda ()
						     (return (string->date str format)))))
					       formats)
				     #f))))
	       (and date (date->time-monotonic
			  (string->date (string-join (map number->string
							  (list (fixup-year (date-year date))
								(date-month date)
								(date-day date)))
						     "/")
					"~Y/~m/~d"))))))))

  (define (money->sxml amount)
    (format "~0,2F" amount))

  (define (empty-string-guard converter)
    (lambda (v die)
      (if (string=? v "")
	  '()
	  (converter v die))))

  (define (null-guard converter)
    (lambda (v)
      (if (null? v)
	  ""
	  (converter v))))

  (define *standard-db-field-types* 'initialized-later-because-of-module-system-letrec-requirement)

  (define (get-db-field-type name)
    (or (hashtable/get *standard-db-field-types* name '#f)
	(error+ "get-db-field-type: missing named field type" name)))

  (define (db-result-class-print-string c)
    (string-append "#<db-result-class "(symbol->string (db-result-class-name c))">"))

  (define (db-result-class-field-names c)
    (hashtable/keys (db-result-class-fields c)))

  (define (db-result-class-field-type c field-name)
    (or (hashtable/get (db-result-class-fields c) field-name '#f)
	(error+ "Missing field type"
		(db-result-class-print-string c)
		field-name)))

  (define (db-select-by-pk class pk-values)
    (let ((path (cons (db-result-class-name class) pk-values))
	  (dbdesc (db-result-class-database-descriptor class)))
      (let loop ((path path)
		 (link (or ((database-descriptor-txn-cache dbdesc))
			   (dbdesc-error "db-select-by-pk needs active transaction" dbdesc))))
	(let ((follow-link (lambda (missing-link-producer)
			     (let ((val (hashtable/get link (car path) 'absent)))
			       (if (eq? val 'absent)
				   (let ((val (missing-link-producer)))
				     (hashtable/put! link (car path) val)
				     val)
				   val)))))
	  (cond
	   ((null? path)
	    (error+ "Internal error: db-select-by-pk - empty path"
		    (db-result-class-print-string class)
		    pk-values))
	   ((null? (cdr path))
	    (follow-link (lambda () ((db-result-class-pk-selector class) class pk-values))))
	   (else
	    (loop (cdr path)
		  (follow-link (lambda () (make-hashtable equal?))))))))))

  (define (db-select class predicates parameters)
    ((db-result-class-selector class) class predicates parameters))

  ;; Select for exactly one result.  If no results are selected, gives #f.
  (define (db-select1 class predicates parameters)
    (let ((results (db-select class predicates parameters))
	  (err (lambda (msg) (error+ msg
				     (db-result-class-print-string class)
				     predicates
				     parameters))))
      (cond
       ((null? results) #f)
       ((null? (cdr results)) (car results))
       (else (err "Too many rows found")))))

  (define (db-save! r . maybe-insert-marker)
    (let ((is-explicit-insert (and (pair? maybe-insert-marker) (car maybe-insert-marker)))
	  (class (db-result-class r)))
      ((db-result-class-saver class) class r is-explicit-insert)))

  (define (make-db-result class . initialisers)
    (make-db-result0 class initialisers))

  (define (copy-db-result r)
    (make-db-result* (db-result-class r)
		     (let ((h (make-hashtable)))
		       (hashtable/for-each (lambda (k v) (hashtable/put! h k v))
					   (db-result-data r))
		       h)))

  (define (make-db-result0 class initialisers)
    (let* ((data (make-hashtable))
	   (initvec (list->vector initialisers))
	   (veclen (vector-length initvec)))
      (if (odd? veclen)
	  (error+ "make-db-result: initialiser list must be even in length" class initialisers)
	  (do ((i 0 (+ i 2)))
	      ((= i veclen) (make-db-result* class data))
	    (hashtable/put! data
			    (vector-ref initvec i)
			    (vector-ref initvec (+ i 1)))))))

  (define (db-result-get r field-name . maybe-absent-thunk)
    (let ((value (hashtable/get (db-result-data r) field-name 'absent)))
      (if (eq? value 'absent)
	  (if (null? maybe-absent-thunk)
	      (error+ "Missing result field"
		      (db-result-class-print-string (db-result-class r))
		      field-name)
	      ((car maybe-absent-thunk)))
	  value)))

  (define (db-result-set! r field-name field-value)
    (hashtable/put! (db-result-data r) field-name field-value)
    r)

  (define (db-result->alist r)
    (cons (db-result-class-name (db-result-class r))
	  (hashtable->alist (db-result-data r))))

  (define (db-result->sxml r)
    (db-result->sxml/join-specs r '()))

  (define basic-join-specs
    (let ((joiner-for (lambda (result-class pk-column nested-join-triples)
			(let ((predicates `((where (= ,pk-column ?pk-value))))
			      (nested-join-specs (basic-join-specs nested-join-triples)))
			  (lambda (pk-value)
			    (map (lambda (result)
				   (db-result->sxml/join-specs result nested-join-specs))
				 (db-select result-class predicates
					    `((pk-value ,pk-value)))))))))
      (lambda (triples)
	(map (lambda (spec)
	       (cons (first spec)
		     (joiner-for (second spec) (third spec) (cdddr spec))))
	     triples))))

  (define (db-result->sxml/join-specs r join-specs-alist)
    (let ((class (db-result-class r))
	  (join-specs (alist->hashtable join-specs-alist)))
      ;; Build a bit of SXML with the class-name as the containing tag.
      (cons (db-result-class-name class)
	    (map (lambda (field-name)
		   (let* ((type (db-result-class-field-type class field-name))
			  (scheme-value (db-result-get r field-name))
			  (sxml-value ((db-field-type-scheme->sxml type) scheme-value))
			  (tail (let ((joiner (hashtable/get join-specs field-name #f)))
				  (if joiner
				      (joiner scheme-value)
				      '()))))
		     `(,field-name ;; tag
		       ,sxml-value ;; actual value
		       ,@tail))) ;; possible dependent rows
		 (db-result-class-field-names class)))))

  ;; Note: ignores any extra children of each field!
  ;;
  (define (sxml->db-result* class sxml absent-field-policy)
    (if (not (eq? (car sxml) (db-result-class-name class)))
	(error+ "sxml->db-result - outer tag mismatch"
		(db-result-class-print-string class)
		sxml)
	(let ((data (make-hashtable))
	      (pathify (lambda (field-name)
			 (string-append (symbol->string (db-result-class-name class))
					"/"
					(symbol->string field-name))))
	      (conversion-error (lambda (msg . args)
				  (apply error+ (string-append "sxml->db-result*: " msg)
					 (db-result-class-print-string class)
					 sxml
					 args))))
	  (with-sxml (list sxml)
	    (for-each (lambda (field-name)
			(let* ((type (db-result-class-field-type class field-name))
			       (sxml-values (select (pathify field-name))))
			  (cond
			   ((null? sxml-values)
			    (case absent-field-policy
			      ((ignore) 'dummy)
			      ((complain)
			       (conversion-error "missing field" field-name))
			      (else
			       (error+ "Invalid absent-field-policy in sxml->db-result*"
				       absent-field-policy))))
			   ((pair? (cdr sxml-values))
			    (conversion-error "duplicate field" field-name))
			   (else
			    (xxexpr-child-case (car sxml-values)
			      (lambda (prefix localname attrs body)
				(let* ((sxml-value (if (null? body)
						       "" ;; empty field = empty string
						       (car body))) ;; here's where we
								    ;; ignore the other kids
				       (value ((db-field-type-sxml->scheme type)
					       sxml-value
					       (lambda (error-message)
						 (error+ (string-append "sxml->db-result*: "
									error-message)
							 sxml-value)))))
				  (hashtable/put! data field-name value)))
			      (lambda (atom) (conversion-error "Bizarre atomic field value"
							       field-name)))))))
		      (db-result-class-field-names class)))
	  (make-db-result* class
			   data))))

  (define (sxml->db-result class sxml)
    (sxml->db-result* class sxml 'complain))

  (define (apply-result-transformer result transformer)
    (cond ((not transformer) result)
	  ((symbol? transformer) (db-result-get result transformer))
	  ((procedure? transformer) (transformer result))
	  (else (error+ "apply-result-transformer: bad transformer"
			transformer
			result))))

  (define (db-results->one-to-one results key-field transformer)
    (let ((table (make-hashtable equal?)))
      (for-each (lambda (result)
		  (hashtable/put! table
				  (db-result-get result key-field)
				  (apply-result-transformer result transformer)))
		results)
      table))

  (define (db-results->one-to-many results key-field transformer)
    (let ((table (make-hashtable equal?)))
      (for-each (lambda (result)
		  (let* ((key (db-result-get result key-field))
			 (entry (hashtable/get table key '())))
		    (hashtable/put! table
				    key
				    (cons (apply-result-transformer result transformer) entry))))
		results)
      table))

  (define (db-result-diff r1 r2)
    (and (eq? (db-result-class r1) (db-result-class r2))
	 (let collect ((remaining-field-names (db-result-class-field-names (db-result-class r1)))
		       (differences '()))
	   (if (null? remaining-field-names)
	       differences
	       (let* ((field-name (car remaining-field-names))
		      (v1 (db-result-get r1 field-name (lambda () 'absent)))
		      (v2 (db-result-get r2 field-name (lambda () 'absent)))
		      (collect1 (lambda (type old new)
				  (collect (cdr remaining-field-names)
					   (cons (list field-name type old new) differences)))))
		 (cond
		  ((equal? v1 v2) (collect (cdr remaining-field-names) differences))
		  ((eq? v1 'absent) (collect1 'added '() v2))
		  ((eq? v2 'absent) (collect1 'removed v1 '()))
		  (else (collect1 'changed v1 v2))))))))

  (define (db-result-grouping-fold group-by-fn accumulator-fn seeds results)
    (let ((keys '())
	  (table (make-hashtable equal?)))
      (for-each (lambda (result)
		  (let ((grouping-key (group-by-fn result)))
		    (let* ((group-row0 (hashtable/get table grouping-key 'absent))
			   (group-row (if (eq? group-row0 'absent)
					  (begin
					    (set! keys (cons grouping-key keys))
					    seeds)
					  group-row0))
			   (new-group-row (apply accumulator-fn result group-row)))
		      (hashtable/put! table grouping-key new-group-row))))
		results)
      (map (lambda (key)
	     (cons key (hashtable/get table key)))
	   (reverse keys))))

  (define make-new-guid-pk 'initialized-later-because-of-module-system-letrec-requirement)

  (define register-table-result-class!
    (let ()
      (define (compile-field-spec field-spec)
	(let ((param (lambda (name def)
		       (cond ((assq name (cddr field-spec)) => cadr)
			     (else def)))))
	  (make-db-table-result-class-field-spec (car field-spec)
						 (get-db-field-type (cadr field-spec))
						 (param 'default #f))))

      (define (build-field-descriptions compiled-specs)
	(let ((fields (make-hashtable)))
	  (for-each (lambda (spec)
		      (hashtable/put! fields
				      (db-table-result-class-field-spec-name spec)
				      (db-table-result-class-field-spec-type spec)))
		    compiled-specs)
	  fields))

      (define (build-columnspecs compiled-specs)
	(map db-table-result-class-field-spec-name compiled-specs))

      (define (pk-selector-for name table-name compiled-specs oid-fields)
	(let ((template `(select distinct
				 (,table-name)
				 ,(build-columnspecs compiled-specs)
				 (where ,@(map (lambda (fn)
						 `(= ,fn ,(string->symbol
							   (string-append "?"
									  (symbol->string fn)))))
					       oid-fields)))))
	  (lambda (class pk-values)
	    (let ((raw-results (run-sql-expr-query (db-result-class-database-descriptor class)
						   (make-sql-expr template)
						   (zip oid-fields pk-values))))
	      (and (pair? raw-results)
		   (make-db-result* class (car raw-results)))))))

      (define (selector-for name table-name compiled-specs)
	(let ((template `(select distinct
				 (,table-name)
				 ,(build-columnspecs compiled-specs))))
	  (lambda (class predicates parameters)
	    (map (lambda (result) (make-db-result* class result))
		 (run-sql-expr-query (db-result-class-database-descriptor class)
				     (make-sql-expr (append template predicates))
				     parameters)))))

      (define (row-for-save class pk-column-names pk-column-values should-insert compiled-specs r)
	(fold (lambda (spec acc)
		(let* ((fieldname (db-table-result-class-field-spec-name spec))
		       (pk-field-index (list-index (cut eq? fieldname <>) pk-column-names)))
		  (if pk-field-index
		      (if should-insert
			  (cons (list fieldname (list-ref pk-column-values pk-field-index)) acc)
			  acc)
		      (let ((fieldval (hashtable/get (db-result-data r) fieldname 'absent)))
			(if (eq? fieldval 'absent)
			    (let ((default-value (db-table-result-class-field-spec-default-value
						  spec)))
			      (if default-value
				  (cons (list fieldname default-value) acc)
				  acc))
			    (cons (list fieldname fieldval) acc))))))
	      '()
	      compiled-specs))

      (define (saver-for name table-name compiled-specs)
	(lambda (class r is-explicit-insert)
	  (let* ((data (db-result-class-data class))
		 (dbdesc (db-result-class-database-descriptor class))
		 (oid-fields (db-table-result-class-data-oid-fields data))
		 (oid-field-values (map (cute hashtable/get (db-result-data r) <> 'absent)
					oid-fields))
		 (need-fresh-pk (cond
				 ((every (cut eq? 'absent <>) oid-field-values) #t)
				 ((any (cut eq? 'absent <>) oid-field-values)
				  (error+ "Primary key partially present in data"
					  oid-fields
					  oid-field-values
					  r))
				 (else #f)))
		 (auto-pk-fn (db-table-result-class-data-auto-pk-fn data))
		 (should-insert (if auto-pk-fn
				    need-fresh-pk
				    is-explicit-insert))
		 (pk (if need-fresh-pk
			 (if auto-pk-fn
			     (auto-pk-fn dbdesc)
			     'primary-key-values-not-available)
			 oid-field-values))
		 (fields (row-for-save class oid-fields pk should-insert compiled-specs r)))
	    (list pk
		  (run-sql-expr-statement dbdesc
					  (make-sql-expr
					   (if should-insert
					       `(insert ,table-name ,fields)
					       `(update ,table-name ,fields
							(where ,@(map (lambda (fn fv)
									`(= ,fn ,fv))
								      oid-fields
								      pk)))))
					  '())))))

      (lambda (database-descriptor name table-name pk-name auto-pk-fn field-specs)
	(let* ((oid-fields (cond
			    ((and (list? pk-name) (every symbol? pk-name)) pk-name)
			    (else (error+ "Bad primary-key name" pk-name))))
	       (compiled-specs (map compile-field-spec field-specs)))
	  (register-result-class!
	   database-descriptor
	   name
	   (make-db-result-class database-descriptor
				 name
				 (make-db-table-result-class-data name
								  table-name
								  oid-fields
								  auto-pk-fn
								  compiled-specs)
				 (build-field-descriptions compiled-specs)
				 (pk-selector-for name table-name compiled-specs oid-fields)
				 (selector-for name table-name compiled-specs)
				 (saver-for name table-name compiled-specs)))))))

  ;; Extracts the PK value from the result of a db-save! call where the
  ;; db-result-class is a table definition - see the result returned by
  ;; the function returned by saver-for in register-table-result-class!
  ;; above.
  ;;
  (define (db-table-save-result-pk result)
    (car result))

  ;; Further common refinement of db-table-save-result-pk, where the
  ;; interesting column in the returned PK (which may be a list of
  ;; multiple column values!) is the first one.
  ;;
  (define (db-table-save-result-pk1 result)
    (first (db-table-save-result-pk result)))

  (define register-view-result-class!
    (let ()
      (define (compile-field-spec field-spec)
	(cons (car field-spec)
	      (get-db-field-type (cadr field-spec))))

      (define (selector-for query)
	(lambda (class predicates parameters)
	  (map (lambda (result) (make-db-result* class result))
	       (run-sql-expr-query (db-result-class-database-descriptor class)
				   (make-sql-expr (append query predicates))
				   parameters))))

      (lambda (database-descriptor name query field-specs)
	(let* ((compiled-specs (map compile-field-spec field-specs)))
	  (register-result-class!
	   database-descriptor
	   name
	   (make-db-result-class database-descriptor
				 name
				 `(view ,name ,query ,field-specs)
				 (alist->hashtable compiled-specs)
				 (lambda _ (error+ "No pk-selector for DB view" name))
				 (selector-for query)
				 (lambda _ (error+ "No saver for DB view" name))))))))

  ;; In general, names (table names, column names) are denoted by
  ;; symbols. Symbols beginning with "?" (eg. "?name") are placeholder
  ;; parameters, for parameterised queries.
  ;;
  ;; <ast>	:== (select <distinct> (<tablespec> ...) (<columnspec> ...) <predicate> ...)
  ;;		|   (update <tablename> ((<columnname> <expr>) ...) <predicate> ...)
  ;;		|   (insert <tablename> ((<columnname> <expr>) ...))
  ;;		|   (delete <tablename> <predicate> ...)
  ;;
  ;; <distinct>	:==				(empty)
  ;;		|   distinct			(a literal keyword)
  ;;
  ;; <columnspec>	:== *				(the symbol spelled "*")
  ;;		|   <expr>
  ;;		|   (as <symbol> <expr>)	(a named value)
  ;;
  ;; <predicate>	:== (where <expr> ...)		(ANDed together)
  ;;		|   (group-by <expr> ...)
  ;;		|   (having <expr> ...)		(ANDed together)
  ;;		|   (order-by (<asc-or-desc> <expr>) ...)
  ;;
  ;; <tablespec>	:== <tablename>
  ;;		|   (as <symbol> <tablename>)	(an aliased table name)
  ;;		|   (<join-kind> <tablespec> (<tablespec> <expr>) ...)
  ;;
  ;; <join-kind>	:== inner-join
  ;;		|   left-outer-join
  ;;
  ;; <expr>	:== <columnname>
  ;;		|   <parameter>
  ;;		|   <string>			(a literal string)
  ;;		|   <number>			(a literal number)
  ;;		|   <literal-sql>		(some literal SQL text to splice in directly)
  ;;		|   (: <expr> ...)		(joins.exprs.with.dots)
  ;;		|   (and <expr> ...)
  ;;		|   (or <expr> ...)
  ;;		... random other SQL functions, see build-expr below ...
  ;;		|   (= <expr> <expr>)		(also for <, >, like, <=, >=, <>)
  ;;		|   (in <expr> (<expr> ...))
  ;;		|   (in <expr> <parameter>)

  (define sql-expr->string
    (let ()
      (define (toplevel port ast parameters)
	(let ()
	  (define (sql-syntax-error message . detail)
	    (apply error+
		   (string-append "sql-expr->string: " message)
		   ast
		   detail))

	  (define (get-parameter parameter)
	    (cond ((assq (parameter-name parameter) parameters) => cadr)
		  (else (sql-syntax-error "missing parameter" parameters parameter))))

	  (define (emit . things)
	    (for-each (lambda (thing) (display thing port)) things))

	  (define (for-each/separator separator fn things)
	    (do ((need-comma #f #t)
		 (things things (cdr things)))
		((null? things))
	      (if need-comma (emit separator))
	      (fn (car things))))

	  (define (for-each/separator&wrap oparen separator cparen fn things)
	    (emit oparen)
	    (for-each/separator separator fn things)
	    (emit cparen))

	  (define (legal-operator? op)
	    (memq op '(= < > <= >= <> like)))

	  (define (legal-sql-function? sqlfn)
	    (memq sqlfn '(sum count avg getdate lower)))

	  (define (parameter-name parameter)
	    (let ((pstr (symbol->string parameter)))
	      (string->symbol (substring pstr 1 (string-length pstr)))))

	  (define (parameter? sym)
	    (and (symbol? sym)
		 (let ((str (symbol->string sym)))
		   (and (> (string-length str) 1)
			(char=? (string-ref str 0) #\?)))))

	  (define (emit-literal-string str)
	    (emit "'")
	    (for-each (lambda (c)
			(if (char=? c #\')
			    (emit "''")
			    (emit c)))
		      (string->list str))
	    (emit "'"))

	  (define (paren-expr expr)
	    (emit "(")
	    (build-expr expr)
	    (emit ")"))

	  (define (build-expr expr)
	    (match expr
	      ((and ,exp ...)		(for-each/separator&wrap "(" " AND " ")" build-expr exp))
	      ((or ,exp ...)		(for-each/separator&wrap "(" " OR " ")" build-expr exp))
	      ((: ,exp ...)		(for-each/separator "." build-expr exp))
	      ((in ,e1 (,e2s ...))	(build-expr e1)
					(emit " IN ")
					(for-each/separator&wrap "(" ", " ")" build-expr e2s))
	      ((in ,e1 ,p) (guard (parameter? p))
					(build-expr e1)
					(emit " IN ")
					(for-each/separator&wrap "(" ", " ")" build-expr
								 (get-parameter p)))
	      ((,operator ,e1 ,e2) (guard (legal-operator? operator))
	       (emit "(") (build-expr e1) (emit ")" operator "(") (build-expr e2) (emit ")"))
	      ((,sqlfn ,exp ...) (guard (legal-sql-function? sqlfn))
	       (emit sqlfn) (for-each/separator&wrap "(" ", " ")" build-expr exp))
	      ((+ ,exp ...) (for-each/separator " + " paren-expr exp))
	      ((- ,exp ...) (for-each/separator " - " paren-expr exp))
	      ((datepart ,part ,exp)
	       (emit "DATEPART(" part ", ") (build-expr exp) (emit ")"))
	      ((datediff ,part ,expr1 ,expr2)
	       (emit "DATEDIFF(" part", ")
	       (build-expr expr1) (emit ", ")
	       (build-expr expr2) (emit ")"))
	      ((cast ,exp ,type)
	       (emit "CAST(") (build-expr exp) (emit " AS " type ")"))
	      ((convert ,datatype ,exp)
	       (emit "CONVERT("datatype", ") (build-expr exp) (emit ")"))
	      ((convert ,datatype ,exp ,style)
	       (emit "CONVERT("datatype", ") (build-expr exp) (emit ", "style")"))
	      (,_ (cond
		   ((parameter? expr)	(build-expr (get-parameter expr)))
		   ((symbol? expr)	(emit "[" expr "]"))
		   ((number? expr)	(emit (number->string expr)))
		   ((string? expr)	(emit-literal-string expr))
		   ((date? expr)	(emit-literal-string
					 (date->string expr "~1")))
		   ((time? expr)	(emit-literal-string
					 (date->string (time-monotonic->date expr)
						       "~1 ~T")))
		   ((null? expr)	(emit "NULL"))
		   ((boolean? expr)	(emit (if expr 1 0)))
		   ((literal-sql? expr)	(emit (literal-sql-expr expr)))
		   (else (sql-syntax-error "Invalid expression" expr))))))

	  (define (build-qualified-name name)
	    (match name
	      ((: ,subname ...)	(for-each/separator "." build-qualified-name subname))
	      (,name		(emit "[" name "]"))))

	  (define (build-columnspecs columnspecs)
	    (for-each/separator ", "
				(lambda (spec)
				  (match spec
				    (*			(emit "*"))
				    ((as ,alias ,exp)	(build-expr exp)
							(emit " AS [" alias "]"))
				    (,exp		(build-expr exp))))
				columnspecs))

	  (define (translate-join-kind join-kind)
	    (case join-kind
	      ((inner-join) "INNER JOIN")
	      ((left-outer-join) "LEFT OUTER JOIN")
	      (else #f)))

	  (define (build-tablespec spec)
	    (match spec
	      ((as ,alias ,name)	(build-qualified-name name)
					(emit " AS [" alias "]"))
	      ((,join-kind ,lhs (,rhs ,exp) ...)
	       (guard (translate-join-kind join-kind))
	       (build-tablespec lhs)
	       (for-each (lambda (rhs1 exp1)
			   (emit " " (translate-join-kind join-kind) " ")
			   (build-tablespec rhs1)
			   (emit " ON ")
			   (paren-expr exp1))
			 rhs exp))
	      (,name			(build-qualified-name name))))

	  (define (build-tablespecs tablespecs)
	    (if (not (null? tablespecs))
		(begin
		  (emit " FROM ")
		  (for-each/separator ", " build-tablespec tablespecs))))

	  (define (build-predicate predicate)
	    (match predicate
	      ((where ,exp ...)		(emit " WHERE ")
					(for-each/separator " AND " build-expr exp))
	      ((group-by ,exp ...)	(emit " GROUP BY ")
					(for-each/separator ", " build-expr exp))
	      ((having ,exp ...)	(emit " HAVING ")
					(for-each/separator " AND " build-expr exp))
	      ((order-by ,clause ...)	(emit " ORDER BY ")
					(for-each/separator ", "
					  (lambda (clause)
					    (match clause
					      ((asc ,exp)	(build-expr exp)
								(emit " ASC"))
					      ((desc ,exp)	(build-expr exp)
								(emit " DESC"))
					      (,else (sql-syntax-error
						      "Bad order-by clause"
						      clause))))
					  clause))
	      (,else (sql-syntax-error "Bad predicate" predicate))))

	  (define (build-predicates predicates)
	    (for-each build-predicate predicates))

	  (define (build-select is-distinct tablespecs columnspecs predicates)
	    (emit "SELECT " (if is-distinct "DISTINCT " ""))
	    (build-columnspecs columnspecs)
	    (build-tablespecs tablespecs)
	    (build-predicates predicates))

	  (define (build-update tablename columnnames exprs predicates)
	    (emit "UPDATE " tablename " SET ")
	    (for-each/separator ", " (lambda (mapping)
				       (emit (first mapping) " = ")
				       (build-expr (second mapping)))
				(zip columnnames exprs))
	    (build-predicates predicates))

	  (define (build-insert tablename columnnames exprs)
	    (emit "INSERT " tablename " ")
	    (for-each/separator&wrap "(" ", " ")" build-expr columnnames)
	    (emit " VALUES ")
	    (for-each/separator&wrap "(" ", " ")" build-expr exprs))

	  (define (build-delete tablename predicates)
	    (emit "DELETE FROM " tablename)
	    (build-predicates predicates))

	  (match ast
	    ((select distinct (,table ...) (,col ...) ,pred ...)(build-select #t table col pred))
	    ((select (,table ...) (,col ...) ,pred ...)		(build-select #f table col pred))
	    ((update ,table ((,col ,exp) ...) ,pred ...)	(build-update table col exp pred))
	    ((insert ,table ((,col ,exp) ...))			(build-insert table col exp))
	    ((delete ,table ,pred ...)				(build-delete table pred))
	    (,else (sql-syntax-error "unsupported query/statement")))))

      (lambda (expr parameters)
	(call-with-output-string
	 (lambda (port)
	   (toplevel port (sql-expr-ast expr) parameters))))))

  (define (sql-result-cursor->list cursor)
    (unfold null?
	    car
	    (lambda (x) (force (cdr x)))
	    (force cursor)))

  (define (run-sql-expr-query database-descriptor expr parameters)
    (sql-result-cursor->list (sql/execute-query (current-connection-to database-descriptor #t)
						(sql-expr->string expr parameters))))

  (define (run-sql-expr-statement database-descriptor expr parameters)
    (sql/execute-update (current-connection-to database-descriptor #t)
			(sql-expr->string expr parameters)))

  ;;===========================================================================
  ;; PART 3: Macros and other high-level user interface
  ;;===========================================================================

  (define-syntax define-database
    (syntax-rules ()
      ((_ dbname connection-code ...)
       (define dbname (make-database-descriptor 'dbname
						(lambda () connection-code ...)
						(make-parameter #f)
						(make-parameter #f)
						(make-hashtable))))))

  (define-syntax define-database-view
    (lambda (stx)
      (syntax-case stx ()
	((_ dbname viewname quasiquery ((fieldname fieldtype param ...) ...))
	 (with-syntax ((varname (splice-symbol (syntax viewname)
					       (syntax dbname) ":" (syntax viewname))))
		      (syntax
		       (define varname
			 (register-view-result-class!
			  dbname 'viewname `quasiquery
			  `((fieldname fieldtype param ...) ...)))))))))

  (define-syntax define-database-table
    (lambda (stx)
      (syntax-case stx ()
	((_ dbname defname (dbtablename (pkcol ...) autopk?) ((fieldname fieldtype param ...) ...))
	 (with-syntax ((varname (splice-symbol (syntax defname)
					       (syntax dbname) ":" (syntax defname))))
		      (syntax
		       (define varname
			 (register-table-result-class!
			  dbname 'defname 'dbtablename '(pkcol ...) autopk?
			  `((fieldname fieldtype param ...) ...)))))))))

  (define (get-jndi-connection name)
    (jdbc/get-connection (jndi/lookup (string-append "java:/comp/env/jdbc/" name))))

  ;;===========================================================================
  ;; PART 4: Static initialisation, to conform to letrec-style for module system
  ;;===========================================================================

  (let ()
    (set! *standard-db-field-types*
	  (let* ((table (make-hashtable))
		 (noop-x->c (lambda (v die) v))
		 (install-type! (lambda (name c->x x->c)
				  (hashtable/put! table name
						  (make-db-field-type name c->x x->c))))
		 (install-null-type! (lambda (name c->x x->c)
				       (install-type! name
						      (null-guard c->x)
						      (empty-string-guard x->c))))
		 (install-date-type! (lambda (name format-string)
				       (install-null-type! name
							   (lambda (d)
							     (date->string (time-monotonic->date d)
									   format-string))
							   (lambda (s die)
							     (or (parse-date s)
								 (die "Invalid date")))))))
	    (install-type! 'string values noop-x->c)
	    (install-null-type! 'int number->string (lambda (v die)
						      (or (string->number v)
							  (die "Invalid numeric format"))))
	    (install-null-type! 'guid values noop-x->c)
	    (install-date-type! 'date "~1")
	    (install-date-type! 'd-month-year-date "~e ~B ~Y")
	    (install-null-type! 'datetime
				(lambda (d) (date->string (time-monotonic->date d) "~1T~T"))
				(lambda (s die)
				  (with/fc
				      (lambda _ (die "Invalid datetime"))
				    (lambda ()
				      (date->time-monotonic
				       (string->date s "~Y-~m-~dT~H:~M:~S"))))))
	    (install-type! 'money money->sxml noop-x->c)
	    (install-type! 'bit
			   (lambda (b) (if b "1" "0"))
			   (lambda (s die)
			     (cond
			      ((equal? s "1") #t)
			      ((equal? s "0") #f)
			      (else (die "Invalid bit")))))
	    table))

    (set! make-new-guid-pk
	  (let ((query (make-sql-expr `(select () ((as id ,(make-literal-sql "NEWID()")))))))
	    (lambda (dbdesc)
	      (list (hashtable/get (car (run-sql-expr-query dbdesc query '())) 'id))))))

  )
