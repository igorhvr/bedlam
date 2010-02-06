(module util/srfi-71
  ((srfi-let i:let i:named-let i:undefined r5rs-let r5rs-let* r5rs-letrec)
   (srfi-let* i:let i:named-let i:undefined r5rs-let r5rs-let* r5rs-letrec)
   (srfi-letrec i:let i:named-let i:undefined r5rs-let r5rs-let* r5rs-letrec)
   uncons unlist unvector values->list values->vector)

  (define-syntax r5rs-let
    (syntax-rules ()
      ((r5rs-let ((v x) ...) body1 body ...)
       (let ((v x) ...) body1 body ...))
      ((r5rs-let tag ((v x) ...) body1 body ...)
       (let tag ((v x) ...) body1 body ...))))

  (define-syntax r5rs-let*
    (syntax-rules ()
      ((r5rs-let* ((v x) ...) body1 body ...)
       (let* ((v x) ...) body1 body ...))))

  (define-syntax r5rs-letrec
    (syntax-rules ()
      ((r5rs-letrec ((v x) ...) body1 body ...)
       (letrec ((v x) ...) body1 body ...))))

  (include "letvalues.scm")
  )
