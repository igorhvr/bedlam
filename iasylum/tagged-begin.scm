;; This comes from PLT - currently not working.

(require-extension (srfi 42))
(require-extension (srfi 1))

(module iasylum/tagged-begin
  (min-length
   max-length
   string->password
   make-password)
  
  (include "password-code.scm")

  (define-syntax let/cc
    (syntax-rules ()
      ((_ k body ...)
       (call/cc (lambda (k) body ...)))))
  (define syntax->list
  (lambda (ls)
    (syntax-case ls ()
      [() '()]
      [(x . r) (cons #'x (syntax->list #'r))])))

  
(define-syntax tagged-begin
  (lambda (stx)    
    (define tag? identifier?)
    (define (non-tag? o) (not (tag? o)))
    
    (define (generate-binding tag-exprs next-tag)
      (match tag-exprs
        [(tag exprs)  (quasisyntax [(unsyntaxx tag) (lambda () (unsyntax-splicing exprs) ((unsyntax3 next-tag)))])]))
    
    (define (generate-last-binding tag-exprs return)
      (match tag-exprs
        [(tag exprs)  (quasisyntax [(unsyntaxy tag) (lambda () (unsyntax-splicing exprs) ((unsyntax4 return) (void)))])]))
    
    (syntax-case stx ()
      [(tagged-begin . tag/exprs-stx)
       (let ([tes (syntax->list (syntax tag/exprs-stx))])
         ; introduce a dummy start-tag, if the tagged-begin starts with an expression
         (when (not (tag? (car tes)))
           (set! tes (cons (syntax start) tes)))
         (let* ([first-tag       (car tes)]
                [tag-exprs-list  ;(list-ec (:pairs p tes)
                                 ;         (if (tag? (car p)))
                                 ;         (list (car p) (take-while non-tag? (cdr p))))
                 (map
                  (lambda (p)
                    (list (car p) (take-while non-tag? (cdr p))))
                  (filter
                   (lambda (l) (tag? (car l)))
                   (let gp ((d tes)) (if (eqv? '() d) '() (cons d (gp (cdr d)))))))
                                 ]
                [tags            (map car tag-exprs-list)])
           ; tag-exprs-list = ( (tag_1 (e1 ...))   (tag_2 (e2 ...)) ... )
           (with-syntax ([go
                          (syntax-local-introduce (syntax go))
                          ;(datum->syntax-object (syntax tagged-begin) (syntax go))
                                 ]
                         [return
                          (syntax-local-introduce (syntax return))
                          ;(datum->syntax-object (syntax tagged-begin) (syntax return))
                                 ])
             (quasi-syntax ((let/cc go
                  (let ([return (lambda (v) (go (lambda () v)))])
                    (letrec
                        ((unsyntax-splicing (map generate-binding 
                                 (drop-right tag-exprs-list 1)
                                 (cdr tags)))
                            (unsyntax2 (generate-last-binding (last tag-exprs-list) (syntax return)))
                            )
                      ((unsyntax first-tag)))))) )
             )))])))

(let ([i 0]) (tagged-begin  loop (set! i (+ i 1))  (if (< i 42) (go loop))  (return i)))
