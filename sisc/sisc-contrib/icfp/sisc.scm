(define (st) (stack-trace (get-last-error)))
(define (et) (print-error (get-last-error)))
  

(import s2j)
(import generic-procedures)
(define-generic repaint)
(define-generic show)
(define gui-frame)
(define gui-pane)
(current-optimizer (lambda (x) x))
(putprop 'assumptive-procedures '*opt* '(+ - * / not car cdr cons))
(import networking) ; Load TCP/IP stack
(import hashtable)

(define-syntax define-alias
  (syntax-rules ()
    ((_ name (args ...) body ...)
     (define-syntax name
       (syntax-rules ()
	 ((_ args ...)
	  body ...))))))


(load "init.scm")
;(define repl go)