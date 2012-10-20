(define channel-map (make-hashtable equal?))
(define (get-channel channel-name)
  (hashtable/get channel-map channel-name))
(define (add-channel channel-name channel-record)
  (hashtable/put! channel-map channel-name channel-record))
(define (remove-channel channel-name)
  (hashtable/remove! channel-map channel-name))

(define (add-user-to-channel channel-name nick)
  (let ([chanrec (get-channel channel-name)])
    (import srfi-1)
    (when chanrec
      (set-channel-members! chanrec
                            (lset-adjoin equal?
                                         (channel-members chanrec)
                                         (cons nick (metaphone nick)))))))


(define (remove-user-from-channel channel-name metaphone-nick)
  (let ([chanrec (get-channel channel-name)])
    (import srfi-1)
    (when chanrec
      (set-channel-members! chanrec (delete metaphone-nick
                                            (channel-members chanrec))))))

(define (channels-user-occupies metaphone-nick)
  (let loop ([keys (hashtable/keys channel-map)])
    (if (null? keys) '()
        (let ([chan (get-channel (car keys))])
          (if (assoc metaphone-nick (channel-members chan))
              (cons (car keys) (loop (cdr keys)))
              (loop (cdr keys)))))))

(define (real-name metaphone-nick)
  (let loop ([keys (hashtable/keys channel-map)])
    (if (null? keys) #f
        (let ([chan (get-channel (car keys))])
          (cond [(assoc metaphone-nick (channel-members chan)) => cdr]
                [else (loop (cdr keys))])))))
