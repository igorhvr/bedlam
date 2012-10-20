(define-syntax mvlet
  (syntax-rules ()
    ((_ () body)
     (let () body))
    ((_ (((lhs* ...) rhs)) body)
     (call-with-values 
         (lambda ()
           rhs)
       (lambda (lhs* ...)
         body)))
    ((_ (((lhs1* ...) rhs1)
         other* ...) body)
     (call-with-values
         (lambda () 
           rhs1)
       (lambda (lhs1* ...)
         (mvlet (other* ...) body))))))
