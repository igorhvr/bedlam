; Manages the game state

(define robot-ids '())
(define robots (make-hashtable))
 
(define robot-locations (make-hashtable))
(define robot-capacities (make-hashtable))
(define robot-loads (make-hashtable))
(define package-owners (make-hashtable))
(define base-package-counts (make-hashtable))

(define (packages-at-base base)
  (hashtable/get! base-package-counts base (lambda () 5)))

(define (packages-at-base! base v)
  (hashtable/put! base-package-counts base v))

(define (robot-capacity id)
  (hashtable/get! robot-capacities id (lambda () 0)))

(define (robot-capacity! id v)
  (hashtable/put! robot-capacities id v))

(define (robot-load id)
  (hashtable/get! robot-loads id (lambda () 0)))

(define (robot-load-incr! id v)
  (when v
	(hashtable/put! robot-loads id (+ (robot-load id) v))))

(define (robot-load-decr! id v)
  (when v
	(hashtable/put! robot-loads id (- (robot-load id) v))))

(define (robot-capacity-remaining id)
  (- (robot-capacity id)
     (robot-load id)))

(define (assign-package! pid rid)
  (hashtable/put! package-owners pid rid))

(define (package-owner pid)
  (hashtable/get package-owners pid 0))

(define (set-robot-x! id x)
  (let ((cl (robot-position id)))
    (store-robot-position! id x (cadr cl))))

(define (set-robot-y! id y)
  (let ((cl (robot-position id)))
    (store-robot-position! id (car cl) y)))

(define (store-robot-position! id x y)
  (hashtable/put! robot-locations id (list x y)))

(define (move-robot! id dx dy)
  (let ((cl (robot-position id)))
    (store-robot-position! id (+ (car cl) dx) (+ (cadr cl) dy))))

(define (robot-position id)
  (if (not (memq id robot-ids))
      (set! robot-ids (cons id robot-ids)))
  (hashtable/get! robot-locations id (lambda () '(1  1))))

(define (reverse-robot-lookup . pos)
  (call/cc (lambda (escape)
	     (hashtable/for-each 
	      (lambda (key val)
		(if (equal? val pos)
		    (escape key)))
	      robot-locations)
	     #f)))
			       
; Parses a response command and updates the gamestate
(define (parse-response id command in)
  (case command
    ((N) (move-robot! id 0 1))
    ((S) (move-robot! id 0 -1))
    ((E) (move-robot! id 1 0))
    ((W) (move-robot! id -1 0))
    (else 
     (let ((arg (read in)))
       (case command
	 ((P) (apply package-pickup! `(,(package-lookup arg) 
                                       ,id ,@(robot-position id))))
	 ((D) (apply package-drop! `(,(package-lookup arg) 
                                     ,id ,@(robot-position id))))
	 ((X) (set-robot-x! id arg))
	 ((Y) (set-robot-y! id arg)))))))
	       
; The distance formula 
;(define dist
;  (letrec ([square (lambda (x) (* x x))])
 ;   (lambda (x1 y1 x2 y2)
  ;    (sqrt (+ (square (- x2 x1))
   ;            (square (- y2 y1)))))))

(define dist
    (lambda (x1 y1 x2 y2)
      (+ (abs (- x1 x2))
         (abs (- y1 y2)))))


