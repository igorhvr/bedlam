; The main turn-based engine

(define (main-loop id last-pos last-move expected-position)
  ; Receive and apply the server responses
  (debug "Receiving server response...")
  (receive-responses id in)

;  (debug "State: ~a [~a]" (robot-position id) (robots-packages id))
  ; If we were making a confrontational move, check the success
  ; and adjust our estimated bid accordingly

  (when (and last-move expected-position)
    (if (and (equal? (cadr last-move) '|Move|)
             (equal? (robot-position id) expected-position))
        (apply set-seen! `(,id ,@last-pos)))
    (if (> 1 (car last-move))
        (if (equal? (robot-position id) expected-position)
            (begin 
              (adjust-bid! #t))
            (begin
              (adjust-bid! #f)))))

  ; Display the board
  (gui id)
;  (render-world id)
;  ; Display player positions
;  (display (let ((mypos (robot-position id)))
;	     (format "I am at: (~a,~a)~%" (car mypos) (cadr mypos))))
;  (for-each (lambda (oid)
;	      (when (not (= oid id))
;		    (display (let ((opos (robot-position oid)))
;			       (format "#~a is at: (~a,~a)~%" 
;				       id (car opos) (cadr opos))))))
;	    robot-ids)
			       
 ; Receive the currently available packages
  (debug "Receiving packages...")
  (let ((pc (receive-packages in id)))
    (if (memq (robot-position id) bases)
        (packages-at-base! (robot-position id) pc))

    (debug "Thinking...")
    ; Decide on our move and send it to the server
    (let ((next-move (decide id (system-time) 900)))
      (debug "Sending move ~a ..." next-move)
      (apply send-command (cons out next-move))
                                        ;loop
      
                                        ; Pause for debugging
                                        ;    (read-char)
      
      (main-loop id (robot-position id)
                 next-move (new-pos (cdr next-move) (robot-position id))))))

