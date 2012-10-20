  (define-syntax define-opt-helper
    (syntax-rules (optional)
      ((define-opt-helper name ((optional (opt-arg opt-init))) (args ...) body ...)
       (define name
	 (lambda (args ... . rest)
	   (let ((opt-arg (if (null? rest)
			      opt-init
			      (car rest))))
	     body ...))))
      ((define-opt-helper name ((optional (opt-arg1 opt-init1) (opt-arg2 opt-init2)))
	 (args ...) body ...)
       (define name
	 (lambda (args ... . rest)
	   (let ((opt-arg1 (if (null? rest)
			       opt-init1
			       (car rest)))
		 (opt-arg2 (if (or (null? rest) (null? (cdr rest)))
			       opt-init2
			       (cadr rest))))
	     body ...))))
      ((define-opt-helper name
	 ((optional (opt-arg1 opt-init1) (opt-arg2 opt-init2) (opt-arg3 opt-init3)))
	 (args ...) body ...)
       (define name
	 (lambda (args ... . rest)
	   (let ((opt-arg1 (if (null? rest)
			       opt-init1
			       (car rest)))
		 (opt-arg2 (if (or (null? rest) (null? (cdr rest)))
			       opt-init2
			       (cadr rest)))
		 (opt-arg3 (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest9)))
			       opt-init3
			       (caddr rest))))
	     body ...))))
      ((define-opt-helper name (arg1 argn ...) (args ...) body ...)
       (define-opt-helper name (argn ...) (args ... arg1) body ...))))

  (define-syntax define-opt
    (syntax-rules (optional)
      ((define-opt (name args ... ) body ...)
       (define-opt-helper name (args ...) () body ...))))