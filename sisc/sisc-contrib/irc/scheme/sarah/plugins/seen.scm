; seen: Note and provide information on a users last message
(define (seen channel message ignore term)
  (if (eqv? #\? (string-ref term (- (string-length term) 1)))
      (set! term (substring term 
                            0 (- (string-length term) 1))))
  (if (equal? ignore "")
      (let-values ([(seen message person) 
                    (lookup-seen dbcon (metaphone term))])
        (if seen (format "~a UTC, saying: ~a."
                              (format (random-elem seen-phrases) person seen)
                              message)
            (random-elem haventseen-responses)))
      'continue))

(define (seenbot channel message)
  (unless (message-is-private? message)
    (store-seen dbcon (message-metaphone-nick message)
                (message-nick message) (message-text message)))
  #t)


(define (lookup-seen conn person)
  (let* ([stmt (jdbc/prepare-statement conn
                (format (string-append "SELECT to_char(timezone('UTC',seenon), 'Mon DD at HH:MI pm')"
		                            ", message, nick FROM seen WHERE id='~a'")
                             person))]
         [result
          (jdbc/execute-query stmt)])
    (if (not (null? result)) 
        (values ((car result) '1)
                ((car result) '2)
		((car result) '3))
        (values #f #f #f))))

(define (store-seen conn id person message)
  (let ([pstmt (if (let-values ([(a b c) (lookup-seen conn id)]) a)
                   (let ([stmt
                          (jdbc/prepare-statement conn 
               "UPDATE seen SET seenon=NOW(), message=?, nick=? WHERE id=?")])
                     (set-string stmt (->jint 1) (->jstring message))
                     (set-string stmt (->jint 2) (->jstring person))
                     (set-string stmt (->jint 3) (->jstring id))
                     stmt)
                   (let ([stmt
                          (jdbc/prepare-statement conn 
                   "INSERT INTO seen(nick,seenon,message, id) VALUES(?,NOW(),?,?)")])
                     (set-string stmt (->jint 1) 
                                 (->jstring person))
                     (set-string stmt (->jint 2) (->jstring message))
                     (set-string stmt (->jint 3) (->jstring id))
                     stmt))])
    (with/fc (lambda (m e) (print-exception (make-exception m e)))
                     (lambda () 
                       (jdbc/execute pstmt)))))


(define haventseen-responses
  '("Sorry, haven't seem 'em."
    "Nope."
    "Sorry, no."
    "Not as far as I can remember."))


(define seen-phrases
  '("I last saw ~a on ~a"
    "~a was here ~a"))
