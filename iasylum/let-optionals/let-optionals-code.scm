;;; Optional argument handling:

;;; Copyright (C) 1996 by Olin Shivers.
;;;
;;; This file defines three macros for parsing optional arguments to procs:
;;; 	(LET-OPTIONALS  arg-list ((var1 default1) ...) . body)
;;; 	(LET-OPTIONALS* arg-list ((var1 default1) ...) . body)
;;; 	(:OPTIONAL rest-arg default-exp)
;;;
;;; The LET-OPTIONALS macro is defined using the Clinger/Rees
;;; explicit-renaming low-level macro system. You'll have to do some work to
;;; port it to another macro system.
;;;
;;; The LET-OPTIONALS* and :OPTIONAL macros are defined with simple
;;; high-level macros, and should be portable to any R4RS system.
;;;
;;; These macros are all careful to evaluate their default forms *only* if
;;; their values are needed.
;;;
;;; The only non-R4RS dependencies in the macros are ERROR 
;;; and CALL-WITH-VALUES.
;;; 	-Olin

;;; (LET-OPTIONALS arg-list ((var1 default1) ...) 
;;;   body
;;;   ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for binding a procedure's optional arguments to either
;;; the passed-in values or a default.
;;;
;;; The expression takes a rest list ARG-LIST and binds the VARi to
;;; the elements of the rest list. When there are no more elements, then
;;; the remaining VARi are bound to their corresponding DEFAULTi values.
;;; It is an error if there are more args than variables.
;;;
;;; - The default expressions are *not* evaluated unless needed.
;;;
;;; - When evaluated, the default expressions are carried out in the *outer*
;;;   environment. That is, the DEFAULTi forms do *not* see any of the VARi
;;;   bindings.
;;;
;;;   I originally wanted to have the DEFAULTi forms get eval'd in a LET*
;;;   style scope -- DEFAULT3 would see VAR1 and VAR2, etc. But this is
;;;   impossible to implement without side effects or redundant conditional
;;;   tests. If I drop this requirement, I can use the efficient expansion
;;;   shown below. If you need LET* scope, use the less-efficient 
;;;   LET-OPTIONALS* form defined below.
;;;
;;; Example:
;;; (define (read-string! str . maybe-args)
;;;   (let-optionals maybe-args ((port (current-input-port))
;;;                              (start 0)
;;;                              (end (string-length str)))
;;;     ...))
;;;
;;; expands to:
;;; 
;;; (let* ((body (lambda (port start end) ...))
;;;        (end-def (lambda (%port %start) (body %port %start <end-default>)))
;;;        (start-def (lambda (%port) (end-def %port <start-default>)))
;;;        (port-def  (lambda () (start-def <port-def>))))
;;;   (if (null? rest) (port-def)
;;;       (let ((%port (car rest))
;;; 	        (rest (cdr rest)))
;;; 	  (if (null? rest) (start-def %port)
;;; 	      (let ((%start (car rest))
;;; 		    (rest (cdr rest)))
;;; 	        (if (null? rest) (end-def %port %start)
;;; 		    (let ((%end (car rest))
;;; 			  (rest (cdr rest)))
;;; 		      (if (null? rest) (body %port %start %end)
;;; 			  (error ...)))))))))


;;; (LET-OPTIONALS args ((var1 default1) ...) body1 ...)

(define-macro (let-optionals arg-list var/defs . body)

  ;; This guy makes the END-DEF, START-DEF, PORT-DEF definitions above.
  ;; I wish I had a reasonable loop macro.

  (define (make-default-procs vars body-proc defaulter-names defs rename)
    (let recur ((vars (reverse vars))
		(defaulter-names (reverse defaulter-names))
		(defs (reverse defs))
		(next-guy body-proc))
      (if (null? vars) '()
	  (let ((vars (cdr vars)))
	    `((,(car defaulter-names)
	       (lambda ,(reverse vars)
		 (,next-guy ,@(reverse vars) ,(car defs))))
	      . ,(recur vars
			(cdr defaulter-names)
			(cdr defs)
			(car defaulter-names)))))))


    ;; This guy makes the (IF (NULL? REST) (PORT-DEF) ...) tree above.

  (define (make-if-tree vars defaulters body-proc rest rename)
    (let recur ((vars vars) (defaulters defaulters) (non-defaults '()))
      (if (null? vars)
;	  `(if (##core#check (null? ,rest))
;	       (,body-proc . ,(reverse non-defaults))
;	       (##sys#error (##core#immutable '"too many optional arguments") ,rest))
	  `(if (null? ,rest)
	       (,body-proc . ,(reverse non-defaults))
	       (error "too many optional arguments"))
	  (let ((v (car vars)))
	    `(if (null? ,rest)
		 (,(car defaulters) . ,(reverse non-defaults))
		 (let ((,v (car ,rest))
		       (,rest (cdr ,rest)))
		   ,(recur (cdr vars)
			   (cdr defaulters)
			   (cons v non-defaults))))))))

  (let* ((vars (map car var/defs))
	 (prefix-sym (lambda (prefix sym)
		       (string->symbol (string-append prefix (symbol->string sym)))))

	 ;; Private vars, one for each user var.
	 ;; We prefix the % to help keep macro-expanded code from being
	 ;; too confusing.
	 (vars2 (map (lambda (v) (gensym (prefix-sym "%" v)))
		     vars))

	 (defs (map cadr var/defs))
	 (body-proc (gensym 'body))

	 ;; A private var, bound to the value of the ARG-LIST expression.
	 (rest-var (gensym '%rest))

	 (defaulter-names (map (lambda (var) (gensym (prefix-sym "def-" var)))
			       vars))

	 (defaulters (make-default-procs vars2 body-proc
					 defaulter-names defs gensym))
	 (if-tree (make-if-tree vars2 defaulter-names body-proc
				rest-var gensym)))

    `(let* ((,rest-var ,arg-list)
	    (,body-proc (lambda ,vars . ,body))
	    . ,defaulters)
       ,if-tree) ) )


;;; (:optional rest-arg default-exp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.

(define-macro (optional rest default-exp)
  (let ([var (gensym)])
    `(let ((,var ,rest))
       (if (null? ,var) 
	   ,default-exp
;	   (if (##core#check (null? (cdr ,var)))
;	       (car ,var)
;	       (##sys#error (##core#immutable '"too many optional arguments") ,var))
	   (if (null? (cdr ,var))
	       (car ,var)
	       (error "too many optional arguments"))
           ))))

(define-macro (:optional . args)	; DEPRECATED to avoid conflicts with keyword-style prefix
  `(optional ,@args) )


;;; (LET-OPTIONALS* args ((var1 default1) ... [rest]) body1 ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is just like LET-OPTIONALS, except that the DEFAULTi forms
;;; are evaluated in a LET*-style environment. That is, DEFAULT3 is evaluated
;;; within the scope of VAR1 and VAR2, and so forth.
;;;
;;; - If the last form in the ((var1 default1) ...) list is not a 
;;;   (VARi DEFAULTi) pair, but a simple variable REST, then it is
;;;   bound to any left-over values. For example, if we have VAR1 through
;;;   VAR7, and ARGS has 9 values, then REST will be bound to the list of
;;;   the two values of ARGS. If ARGS is too short, causing defaults to
;;;   be used, then REST is bound to '().
;;; - If there is no REST variable, then it is an error to have excess
;;;   values in the ARGS list.

(define-macro (let-optionals* args var/defs . body)
  (let ([rvar (gensym)])
    `(let ((,rvar ,args))
       ,(let loop ([args rvar] [vardefs var/defs])
	  (if (null? vardefs)
;	      `(if (##core#check (null? ,args))
;		   (let () ,@body)
;		   (##sys#error (##core#immutable '"too many optional arguments") ,args) )
	      `(if (null? ,args)
		   (let () ,@body)
		   (error '"too many optional arguments"))
	      (let ([head (car vardefs)])
		(if (pair? head)
		    (let ([rvar2 (gensym)])
		      `(let ((,(car head) (if (null? ,args) ,(cadr head) (car ,args)))
			     (,rvar2 (if (null? ,args) '() (cdr ,args))) )
			 ,(loop rvar2 (cdr vardefs)) ) )
		    `(let ((,head ,args)) ,@body) ) ) ) ) ) ) )