(define (instrument annotation nexp lvars depth reg) 
  (let ([uid (gensym)])
    `(call/cc (lambda (k)
                (begin
                  (debugger-hook ',uid k ',annotation ',lvars
                                 (lambda (%x . %args)
                                   ,(lvar-dispatch lvars))
                                 ,depth)
                ,(if annotation 
                     (let-values ([(line col file)
                                   (split-source-tracking annotation)])
                       (register-ipoint file line col uid)
                       `(compile-in-annotation ,nexp ,annotation))
                     nexp))))))

(define (lvar-dispatch lvars)
  (let loop ([acc '(error 'debugger '"No such lexical: ~a" %x)]
             [x lvars])
    (cond [(null? x) acc]
          [else 
           (loop `(if (eq? %x ',(car x))
                      (if (null? %args) ,(car x) (set! ,(car x) 
                                                       (car %args)))
                      ,acc)
                 (cdr x))])))
