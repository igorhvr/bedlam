;; Circular lists

(module circular-list
    (make-clist add-before! add-after! clist-ref clist-next clist-last
                clist-value 
                clist-empty? clist-remove! clist-remove-next! 
                clist-remove-last!
                clist-length clist->list clist-map clist-for-each)
  (define-syntax before
    (syntax-rules ()
      ((_ cl)
       (cadr cl))))
  (define-syntax after
    (syntax-rules ()
      ((_ cl)
       (cddr cl))))
  (define-syntax value
    (syntax-rules ()
      ((_ cl)
       (car cl))))
  (define-syntax set-before!
    (syntax-rules ()
      ((_ cl v)
       (set-car! (cdr cl) v))))
  (define-syntax set-after!
    (syntax-rules ()
      ((_ cl v)
       (set-cdr! (cdr cl) v))))
  (define-syntax set-value!
    (syntax-rules ()
      ((_ cl v)
       (set-car! cl v))))
  (define-syntax first
    (syntax-rules ()
      ((_ cl)
       (vector-ref cl 1))))
  (define-syntax last
    (syntax-rules ()
      ((_ cl)
       (vector-ref cl 2))))
  (define-syntax set-first!
    (syntax-rules ()
      ((_ cl v)
       (vector-set! cl 1 v))))
  (define-syntax set-last!
    (syntax-rules ()
      ((_ cl v)
       (vector-set! cl 2 v))))

  (define (make-unlinked-cell val)
    (cons val (cons #f #f)))
                              
  (define (add-cell-after! v b)
    (let* ([a (after b)]
           [cell (cons v (cons b a))])
          (set-before! a cell)
          (set-after! b cell)))

  (define (remove-cell! c)
    (let ([b (before c)]
          [a (after c)])
      (set-after! b a)
      (set-before! a b)))

  (define (make-clist)
    (let ([cell (make-unlinked-cell 
                 (string->uninterned-symbol "circular-list"))])
      (set-before! cell cell)
      (set-after! cell cell)
      cell))
  (define (clist-value cl)
    (value cl))
  (define (clist-empty? cl)
    (eq? (before cell) cell))
  (define (clist-last cl)
    (before cl))
  (define (clist-next cl)
    (after cl))
  (define (add-before! cl v)
    (add-cell-after! v (before cl)))
  (define (add-after! cl v)
    (add-cell-after! v cl))
  (define (clist-ref cl n)
    (if (zero? n) 
        cl
        (clist-ref (after cl) (- n 1))))
  (define (clist-remove! cl v)
    (let loop ([current (after cl)])
      (cond [(equal? (value current) 'circular-list)
             (void)]
            [(equal? v (value current))
             (let ([next (after current)])
                (remove-cell! current)
                (loop next))]
            [else (loop (after current))])))
  (define (clist-remove-next! cl)
    (remove-cell! (after cl)))
  (define (clist-remove-last! cl)
    (remove-cell! (before cl)))
  (define (clist-length cl)
    (let loop ([current (after cl)] [acc 0])
      (cond [(equal? (value current) 'circular-list)
             acc]
            [else (loop (after current) (+ acc 1))])))
  (define (clist-map proc cl)
    (let loop ([current (after cl)])   
      (cond [(equal? (value current) 'circular-list)
             '()]
            [else (cons (proc (value current))
                        (loop (after current)))])))
  (define (clist-for-each proc cl)
    (let loop ([current (after cl)])   
      (unless (equal? (value current) 'circular-list)
        (proc (value current))
        (loop (after current)))))
  (define (clist->list cl)
    (clist-map (lambda (x) x) cl)))
  
  