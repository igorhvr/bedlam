(import pattern-matching)
(import hashtable)

(define (enable-debugging)
  (emit-debugging-symbols #t)
  (emit-annotations #t)
  (current-optimizer debugging-instrument))

(define (debugging-instrument expr)
  (let* ([reg (make-hashtable)]
         [res (scan expr '() 0 #f reg)])
    (hashtable/for-each (lambda (key value)
                          (register-ipoint (caddr value) (car value)
                                           (cadr value) key))
                        reg)
    res))

(define (make-proper p)
  (cond [(null? p) '()]
        [(not (pair? p)) (list p)]
        [else (cons (car p) (make-proper (cdr p)))]))

(define (split-source-tracking annotation)
  (values (cond [(assq 'line-number annotation) => cdr] [else #f])
          (cond [(assq 'column-number annotation) => cdr] [else #f])
          (cond [(assq 'source-file annotation) => cdr] [else #f])))

(define (scan expr lvars depth annotations reg)
  (match expr
    (,x 
     (guard (symbol? x))
     x)
    ((quote ,x) 
     `',x)
    ((begin ,[exp1] ,[exps*] ...)
     `(begin ,exp1 ,@exps*))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    (((lambda ,formals ,body) ,[values*] ...)
     (let ([nbody (scan body (append (make-proper formals) lvars) (+ depth 1)
                        #f reg)])
       `((lambda ,formals ,nbody) ,@values*)))
    ((lambda ,formals ,body) 
     (let ([nbody (scan body (append (make-proper formals) lvars) 
                        (+ depth 1) #f reg)])
       `(lambda ,formals ,nbody)))
    ((letrec ((,lhs* ,rhs*) ...) ,body)
     (let ([nlv (append lvars lhs*)])
       (let ([nrhs* (map (lambda (rhs) (scan rhs nlv (+ depth 1) #f reg)) 
                         rhs*)]
             [nbody (scan body nlv (+ depth 1) #f reg)])
         `(letrec ((,lhs* ,nrhs*) ...) ,nbody))))
    ((define ,lhs ,[rhs]) 
     `(define ,lhs ,rhs))
    ((set! ,formal ,[value]) 
     `(set! ,formal ,value))
    ((compile-in-annotation ,expr ,annotation)
     (let ([scr (scan expr lvars depth annotation reg)])
       (if (and (pair? scr) (eq? (car scr) 'call/cc))
           scr
           `(compile-in-annotation ,scr ,annotation))))
    ((,rator ,rands* ...)
     (instrument annotations `(,(scan rator lvars (+ depth 1) #f reg)
                              ,@(map (lambda (e)
                                       (scan e lvars (+ depth 1) #f reg))
                                     rands*)) lvars depth reg))
    (,other (error 'optimizer "Unrecognized s-expression: ~a" other))))


