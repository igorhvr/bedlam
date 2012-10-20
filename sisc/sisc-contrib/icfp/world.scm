; World Representation

(define world-width (void))
(define world-height (void))
(define world-scale (void))
(define world-map (void))
(define world (void))

(define bases '())
(define (add-home-base! x y)
  (set! bases (cons (list x y) bases)))

(define (make-world! width height)
  (let ((world (make-hashtable)))
    (set! world-width width)
    (set! world-height height)
    (set! world-scale (+ width height))
    (set! world-map (make-string (* width height) #\.))))

(define (world-idx x y)
  (+ -1 x (* (- y 1) world-width)))

(define (world-ref x y)
  (string-ref world-map (world-idx x y)))

(define (world-set! wrl x y v)
  (string-set! world-map (world-idx x y) v))

(define (do-nothing . args) (void))

(define (world-iterator proc-row
			proc-col-pre proc-col-post)
  (lambda (world)
    (do ((y world-height (- y 1)))
	((zero? y))
      (proc-col-pre y)
      (do ((x 1 (+ x 1)))
	  ((> x world-width))
	(proc-row x y))
      (proc-col-post y))))

    
(define (render-world id)
  ((world-iterator (lambda (x y)
		     (if (equal? (list x y) (robot-position id))
			 (display #\*)
			 (display (world-ref x y))))
		   do-nothing
		   (lambda (y) (newline))) world))

(define (water? x y)
  (eq? (world-ref x y) WATER))

(define (wall? x y)
  (eq? (world-ref x y) WALL))

(define (opponent? id x y)
  (let ((rob (reverse-robot-lookup x y)))
    (and rob (not (= rob id)))))


(define (new-pos move old-pos)
  (let ((old-x (car old-pos))
	(old-y (cadr old-pos)))
    (if (eq? (car move) '|Move|)
        (let ((np
               (case (cadr move)
                 ((|N|) (list old-x (+ old-y 1)))
                 ((|S|) (list old-x (- old-y 1)))
                 ((|E|) (list (+ old-x 1) old-y))
                 ((|W|) (list (- old-x 1) old-y)))))
          (if (apply wall? np)
              (list old-x old-y)
              np))
	(list old-x old-y))))


