; Contains the 'brain' of the robot.  Here we use part of our 
; available time (800ms) to decide on what to do next

; Thoughts
;
; Our robot can be in one of three modes
; 
; 1. Motion 
; ---------
; 
; The bot will ordinarily try to deliver the closest package
; first, under the assumption that its better to get a package
; delivered even if its lighter.
; 
; If two packages we carry are of equal weight (with a possible
; fuzz factor) the we'll try to move in the direction of the heaviest.
;
; 2. Confrontation
; ----------------
; 
; Things get more complicated if we're near a robot.  
; If we're immediately adjacent to a robot, we
; have two choices. (Well, three really, but our robot is never going
; to chicken out.)  
;  
; 1] Bid on a push move into the robot.  We'll probably bid higher
;    than usual, so we have a better chance pushing instead of being pushed.
; 2] Sidestep the oncoming robot.  That way we dont get pushed if we
;    happen to move first.  Again, we'll try to bid higher.  A sidestep
;    will be a move parallel to the direction of our preferred travel 
;    (based on the package we want to deliver).
;
; We'll bias towards 1 and increase the bid even further if the push
; will kill the other robot.  Furthermore, if we're in real trouble,
; we need to bias towards 2 (say if two robots have us cornered) and
; also bid higher.
;
; 3. Package Management
; ---------------------
;
; We want to maintain an optimal load, with a heuristic that boosts
; the perceived value of close packages and penalizes far ones.  The
; distance will be a scaling factor applied against the packages weight.
; Obviously, we'll wan't to keep close to fully loaded as well (with
; respect to capacity
;
; Bidding
; -------
;  We'll only increase our bid if we're near a robot, otherwise we bid 1
;  each time to conserve money.  How much we'll bid will have to be 
;  determined by how many occasions where we bid higher turned out in our
;  favor.  If something goes our way, we'll lower our bid next time we're
;  in that situation.  If it doesn't, we'll increase future bids.  
;
; Decision Algorithm
; ------------------
; Each turn, we'll enumerate our available moves, in order of:
; 
;  1. Confrontational Moves
;  2. Motion Moves
;  3. Package Management Moves
; 
; Then, we'll proceed through the available moves, calculating a
; perceived benefit to the move.  If we run out of time, we simply
; pick the highest benefit move we've seen so far (for the most part,
; we'll be able to calculate the benefit of all available moves within
; the allotted time).  
;
; Once we've selected a move (or run out of time), we go into the bidding
; algorithm.  If the move is a motion or package management move, we'll
; simply bid 1.  Otherwise we have to look at our estimated successful
; bid and use that.
; 
; Next, we submit our move, and see what happens.
; When we get the response, we update our successful bid value if 
; we attempted a confrontational move.  

(define last-state #f)

(define (decide id start-time limit)
  (let ((current-pos (robot-position id)))
    (mvlet ([(selected-move state) 
             (if (and (not new-packages) last-state)
                 (last-state current-pos start-time)
                 (begin (set! new-packages #f)
                        (call/cc (lambda (return)
                        (a* id current-pos start-time limit return)))))])
       (begin
         (set! last-state (if (eq? (car selected-move)
                                   '|Move|)
                              state #f))
         (attach-bid selected-move id (car current-pos) 
                     (cadr current-pos))))))

(define (attach-bid move id x y)
  (cons (if (> (neighbor-search 1xmap id x y opponent? 1) 0)
	    (forceful-bid id #t)
	    (peaceful-bid id))
	move))

(define (pick-move moves)
  (let loop ((best-move (car moves))
	     (rest (cdr moves)))
    (cond [(null? rest) best-move]
	  [(< (car best-move) (caar rest))
	   (loop (car rest) (cdr rest))]
	  [(> (car best-move) (caar rest))
	   (loop best-move (cdr rest))]
	  [else (loop (if (zero? (random 2)) best-move (car rest))
		      (cdr rest))])))

(define enumerate-moves 
  (let ((delta-map  '(           (+0 +1 |N|)
		      (-1 +0 |W|)           (+1 +0 |E|)
		                 (+0 -1 |S|)         )))
    (lambda (id x y)
      (append 
      ; Motion moves
       (apply append
	      (map (lambda (d)
		     (let ((nx (+ (car d) x))
			   (ny (+ (cadr d) y)))
		       (if (and (<= 1 nx world-width)
				(<= 1 ny world-height)
                                (not (or (wall? nx ny) (water? nx ny))))
			   `((|Move| ,(caddr d)))
			   '())))
		   delta-map))
;       '((|Drop|))
       ; Package pickup moves
       (let ploop ((ph (packages x y)) 
                  (acc '())
                  (acct '())
                  (acw (robot-capacity-remaining id)))
         (cond [(null? ph)
                (if (null? acc)
                    '()
                    `((|Pick| ,@acc)))]
               [(< (package-weight (car ph)) acw)
                (ploop (cdr ph) (cons (package-id (car ph)) acc) acct
                      (- acw (package-weight (car ph))))]
               [(and (null? acc) (> (package-weight (car ph))
                                    acw))
                (ploop (cdr ph) acc acct acw)]
               [(null? acc)
                (ploop (cdr ph) '() '() (robot-capacity-remaining id))]
               [else (ploop ph '() (cons `(|Pick| ,@acc) acct) 
                            (robot-capacity-remaining id))]))

       ; Package drop moves
       (let dloop ((ph (robots-packages id)) 
                  (acc '()))
         (cond [(null? ph)
                (if (null? acc)
                    '()
                    `((|Drop| ,@acc)))]
               [(equal? (package-destination (car ph)) (list x y))
                (dloop (cdr ph) (cons (package-id (car ph)) acc))]
               [else 
                (dloop (cdr ph) acc)]))))))



(define calculate-fitness 
(letrec
    ((get-cost
      (lambda (cache seen id x y . move)
        (cond [(hashtable/get cache (make-rectangular x y)) => (lambda (x)
                                                     x)]
              [else 
                (let ((cost
                       (apply fitness 
                              `(,id ,seen ,x ,y ,@move))))
                  (hashtable/put! cache (make-rectangular x y) cost)
                  cost)]))))

  (lambda (cache seen id pos move)
  (case (car move)
    ((|Drop|)
     (if (null? (cadr move))
         (weight id 'do-nothing)
         (apply + (map (lambda (p)
                         (let ((dist-to-deliver 
                                (apply dist `(,@pos
                                              ,@(package-destination p)))))
                           (if (zero? dist-to-deliver)
                               (weight id 'delivery)
                               (weight id 'go-nowhere))))
                       (map package-lookup (cdr move))))))
    ((|Pick|)
     (if (null? (cadr move))
         (weight id 'go-nowhere)
         (apply + (map (lambda (p)
                         (let ((dist-to-deliver 
                                (apply dist `(,@pos
                                              ,@(package-location p))))
                               (wght (package-weight p)))
                           (* (weight id 'pickup) world-scale
                              (/ wght (zeroguard dist-to-deliver)))))
                       (map package-lookup (cdr move))))))
    ((|Move|)
     (let* ((myx (car pos))
	    (myy (cadr pos)))
       (case (cadr move)
	 ((|N|) (if (= myy world-height) 
		    (weight id 'go-nowhere)
		    (get-cost cache seen id myx (+ myy 1) move)))
	 ((|S|) (if (= myy 1) 
		    (weight id 'go-nowhere)
		    (get-cost cache seen id myx (- myy 1) move)))
	 ((|E|) (if (= myx world-width) 
		    (weight id 'go-nowhere)
		    (get-cost cache seen id (+ myx 1) myy move)))
	 ((|W|) (if (= myx 1) 
		    (weight id 'go-nowhere)
		    (get-cost cache seen id (- myx 1) myy move))))))
    (else 0.0)))))

(define (fitness id seen x y move)
  (all-weights id seen x y move))

(define (xdif direction current-x)
  (case direction
    ((N) (+ current-x 1))
    ((S) (- current-x 1))
    (else current-x)))

(define (ydif direction current-y)
  (case direction
    ((E) (+ current-y 1))
    ((W) (- current-y 1))
    (else current-y)))
