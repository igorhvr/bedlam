; A* Algorithm

;(define (pq-create) (make-heap (lambda (e1 e2)
;				 (> (node-cost e1)
;				    (node-cost e2)))))
;(define pq-add! heap-insert!)
;(define pq-max! heap-extract-max!)
;(define pq-empty? (lambda (pq)
;		    (= (heap-length pq) 0)))

(define node-pos caddar)
(define node-move cadar)
(define node-cost caar)
(define node-parent cdr)
(define (set-cost! f v)
  (set-car! (car f) v))

(define (make-node move parent . pos)
  `((0 ,move ,(if (not (null? parent))
		  (new-pos move (node-pos parent))
		  pos)) . ,parent))

(define (fetch-successors id node)
  (if (package-move? (node-move node))
      '()
      (let ((possible-moves (apply enumerate-moves `(,id ,@(node-pos node))))
            (parent-pos (and (not (null? (node-parent node)))
                             (node-pos (node-parent node))))
            (this-pos (node-pos node)))
        (map (lambda (m p)
               (apply make-node `(,m ,node ,@p)))
             possible-moves
             (map (lambda (v) (new-pos v (node-pos node)))
                  possible-moves)))))
	
	     

;(trace 'fetch-successors)

(define last-path '())

(define (identity x) x)
(define a* 
  (letrec ((unwind 
	    (lambda (return state id start next c)
              ;Hmm, ran out of moves
              (cond [(null? (node-parent next))
                     (begin (clear-seen! id)
                            '(|Drop|))]
                    [(equal? start (node-pos (node-parent next)))
                     (let ((n (length next)))
                       (return (node-move next) 
                               #f #;state))]
                    [else (unwind return #f #;state id start (node-parent next)
                                  (+ c 1))])))
           (highest-rated 
            (lambda (cache id closed-list max-so-far maxf-so-far)
              (if (null? closed-list)
                  max-so-far
                  (let ((fitness (calculate-fitness 
                                  cache (node-parent (car closed-list)) id 
                                  (node-pos (car closed-list))
                                  (node-move (car closed-list)))))
                    (if (> fitness maxf-so-far)
                        (highest-rated cache id 
                                       (cdr closed-list)
                                       (car closed-list)
                                       fitness)
                        (highest-rated cache id 
                                       (cdr closed-list)
                                       max-so-far
                                       maxf-so-far)))))))
    (lambda (id start start-time limit return)
      (let ((openl (pq-create))
            (cost-cache (make-hashtable))
            (last-move #f)
            (moves 0)
            (closed '())
            (root-node (apply make-node `((false) () ,@start)))
            (make-restart
             (lambda (k)
               (lambda (npos nstart)
                 (set! start-time nstart)
                 (set! start npos)
                 (k)))))
                                        ;           (debug "Root node: ~a" root-node)
 ;          (debug "Open: ~a" openl)
	   (pq-add! openl root-node 0.0)
	   (let search-loop ()
;             (debug "open: ~a" openl)
;             (debug "closed: ~a" closed)
	     (if (pq-empty? openl)
                 (unwind return #f id start
                         (if (null? closed)
                             last-move
                             (begin 
                               (set-cost! root-node -100000.0)
                               (let ((highest (highest-rated cost-cache id
                                                             closed root-node 
                                                             -100000.0)))
                                 (set! last-path (map node-pos highest))
                                 highest))) 0)
		 (let ((node-current
			(pq-remove-max! openl)))
;                   (debug "Exploring: ~a" node-current)
                   (set! moves (+ moves 1))
		   (if (> (- (system-time) start-time) limit)
					; Times up
		       (begin 
                         (debug "Stopping search after ~a moves, ~a ms"
                                moves (- (system-time) start-time))
                         (set-cost! root-node -100000.0)
                         (let ((selected-node
                                (highest-rated cost-cache id
                                               (cons node-current closed)
                                               root-node -100000.0)))
                           (set! last-path (let loop ((n selected-node))
                                             (if (null? n) '()
                                                 (cons (node-pos n)
                                                       (loop (node-parent n))))))
                           (call/cc (lambda (k)
                                      (unwind return (make-restart k)
                                              id start selected-node 0))))))
                       (begin
                         (let loop ((s (fetch-successors id node-current)))
                           (unless (null? s)
                               (let ((f (car s)))
                                 (call/cc 
                                  (lambda (e)
                                    (let* ((parent-cost (node-cost node-current))
                                           (my-cost (+ parent-cost -0.0001))
                                           (goodness 
                                            (calculate-fitness 
                                             cost-cache 
                                             (node-parent node-current)
                                             id 
                                             (node-pos node-current) 
                                             (node-move f)))
                                           (weight (+ my-cost goodness)))
;        (debug "Examining [~a+~a=~a] ~a..." 
;               (node-cost node-current)
;               (apply calculate-fitness
;                      `(,id ,(node-pos node-current) ,(node-move f)))
;               cost f)

                                      (for-each (lambda (s2)
                                               (if (and 
                                                     (equal? (node-pos s2)
                                                             (node-pos f))
                                                     (equal? (node-move s2)
                                                             (node-move f))
                                                     (>= (node-cost s2) 
                                                         my-cost))
                                                   (e (loop (cdr s)))))
                                                (cdr s))
                                      (for-each (lambda (s2)
                                                (if (and 
                                                     (equal? (node-pos s2)
                                                             (node-pos f))
                                                     (equal? (node-move s2)
                                                             (node-move f))
                                                     (>= (node-cost s2) 
                                                         my-cost))
                                                    (e (loop (cdr s)))))
                                                closed)
                                      (set-cost! f my-cost)
                                      (pq-delete! openl f)
                                      (pq-add! openl f weight))
                                    (set! last-move f)
                                    (set! closed (remove f closed))
                                    (loop (cdr s))))))))
                   (set! closed (cons node-current closed))
                   (search-loop))))))))

  (define (test-a* x y) 
  (store-robot-position! 1 x y)
  (write (a* 1 (list x y) (system-time) 1500))
  (newline)
  (render-world 1))
  

;(test-a* 5 5)