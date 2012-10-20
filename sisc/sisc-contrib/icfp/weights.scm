;; Builds up a floating point value summing a given weight
;; divided by distance for each cell that satisfies a provided 
;; predicate.
(define 2xmap
	 '((-2 -2 4) (-1 -2 3) (0 -2 2) (1 -2 3) (2 -2 4)
	   (-2 -1 3) (-1 -1 2) (0 -1 1) (1 -1 2) (2 -1 3)
	   (-2 0 2)  (-1 0 1)           (1 0 1)  (2 0 2)
	   (-2 1 3)  (-1 1 2)  (0 1 1)  (1 1 2)  (2 1 3)
	   (-2 2 4)  (-1 2 3)  (0 2 2)  (1 2 3)  (2 2 4)))

(define 1xmap 
  '(         (0 -1 1) 
    (-1 0 1)         (1 0 1)  
             (0 1 1)  ))

(define neighbor-search
  (lambda (dmap id x y pred? value)
    (apply + (map (lambda (d)
		    (let ((nx (+ (car d) x))
			  (ny (+ (cadr d) y)))
		      (if (and (<= 1 nx world-width)
			       (<= 1 ny world-height)
			       (pred? id nx ny))
			  (/ value (caddr d))
			  0)))
		  dmap))))

(define weights (make-hashtable))
(define (set-weight! id type val)
  (hashtable/put! (hashtable/get! weights id 
				  (lambda () (make-hashtable)))
		  type val))

(define initial-weights
  '((return-to-base . 5)
    (danger . -2)
    (crowd . -0.1)
    (delivery . 1000)
    (delivery-transit . 10)
    (do-nothing . -0.1)
    (revisit . -3)
    (visit . 1)
    (pickup . 200)
    (go-nowhere . -100)))

(define (weight id type)
  (hashtable/get (hashtable/get! weights id make-hashtable)
		  type (cdr (assq type initial-weights))))

(define (danger-weight id seen x y)
  (neighbor-search 1xmap id x y (lambda (id x y) (water? x y)) 
		   (weight id 'danger)))

(define (crowd-weight id seen x y)
  (neighbor-search 2xmap id x y opponent? (weight id 'crowd)))

(define (revisit-weight id seen x y)
  (let ((sum 
         (apply + (map (lambda (d)
                         (let ((nx (+ (car d) x))
                               (ny (+ (cadr d) y)))
                           (if (and (<= 1 nx world-width)
                                    (<= 1 ny world-height))
                               (/ (+ (seen? id x y)
                                     (sseen? seen (list nx ny))) (+ 1 (caddr d)))
                               0)))
                       (cons '(0 0 0.5) 2xmap)))))
    (- (* sum sum))))

(define (delivery-distance-weight id seen x y)
  (let ((delivery-weight (weight id 'delivery)))
    (let loop ((packages (robots-packages id)) (acc 0.0))
      (if (null? packages) acc
          (let ((visargs `(,x ,y ,@(package-destination (car packages)))))
            (loop (cdr packages)
                  (+ (*
                      (if (apply visible? visargs) 3 0.5)
                    (/ (package-weight (car packages))
                       (zeroguard (apply dist visargs)))
                    (weight id 'delivery-transit))
		   acc)))))))

(define (all-unclaimed-packages)
  (hashtable/map (lambda (key val) val) unclaimed-packages))

(define (zeroguard n) (if (zero? n) 1 n))

(define (pickup-distance-weight id seen x y)
  (let ((pickup-weight (weight id 'pickup)))
    (let loop ((packages (all-unclaimed-packages)) (acc 0.0))
      (if (null? packages) acc
	  (loop (cdr packages)
		(+ (/ pickup-weight 
		      (zeroguard (apply dist `(,x ,y ,@(package-location (car packages))))))
		   acc))))))

(define (base-weight id seen x y)
  (apply + (map (lambda (base)
		  (let ((rp (robots-packages id))
                        (visargs `(,x ,y ,@base))
                        (pab (packages-at-base base)))
                    (if (zero? pab)
                        0
                        (/ (/ (* pab
                                 (weight id 'return-to-base))
                              (let ([x (apply dist visargs)])
                                (if (zero? x) 1 x)))
                           (+ 1 (length rp))
                           (if (apply visible? visargs)
                               0.33
                               2)))))
		bases)))

(define (search-weight id seen x y)
  (* (weight id 'revisit)
     (if (null? (robots-packages id))
         (seen? id x y)
         0)))

(define (barrier-weight id seen x y)
  (if (wall? x y)
      (weight id 'go-nowhere)
      0))

(define visible? 
  (letrec ((vh 
            (lambda (x1 y1 x2 y2 f)
              (cond [(wall? x1 y1) #f]
                    [(water? x1 y1) #f]
                    [(and f (= x1 x2))
                     (cond [(= y1 y2) #t]
                           [else (vh x1 (+ y1 (- y2 y1)) x2 y2 (not f))])]
                    [(= y1 y2)
                     (cond [(= x1 x2) #t]
                           [else (vh (+ x1 (- x2 x1)) y1 x2 y2 (not f))])]))))
    (lambda (x1 y1 x2 y2)
      (vh x1 y1 x2 y2 #f))))

(define (all-weights id seen x y . move)
  (apply + (let ((result
		  (map (lambda (v)
			 (v id seen x y)) 
		       (list danger-weight crowd-weight 
			     ;pickup-distance-weight 
                             revisit-weight
                             delivery-distance-weight
			     ;search-weight 
                             base-weight
			     #;barrier-weight))))
;	     (debug "Resulting weights for move ~a to ~a: ~a [~a]" 
;                    move (list x y) result (apply + result))
	     result)))