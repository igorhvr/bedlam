;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (default-page)
;  (let* ([n1 (read-number 0)]
;         [n2 (read-number 1)])
;    (display-result (+ n1 n2))))

(define (default-page)
  (display-result
   (apply + (map read-number (iota (read-number "of numbers"))))))

(define (read-number p)
  (display-form "number" `((position . ,p)))
  (string->number (get-param 'number)))

(define (display-result n)
  (display-page "result" `((result . ,n))))

(define (calculator servlet)
  (let ([get/post-fn (java-wrap (make-dispatcher default-page))])
    (for-each (lambda (m) (m servlet get/post-fn))
              (list :get-fn! :post-fn!))))
