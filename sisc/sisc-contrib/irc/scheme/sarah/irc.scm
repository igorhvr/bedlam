(define-java-class <java.util.string-tokenizer>)
(define-generic-java-methods next-token has-more-tokens)

(define (filter-specials str)
  (let loop ([ls (string->list str)] [acc '()])
    (cond [(null? ls) (list->string (reverse acc))]
          [(and (char<? (car ls) #\space)
                (not (char=? (car ls) #\newline)))
           (loop (cdr ls) acc)]
          [else (loop (cdr ls) (cons (car ls) acc))])))

(define (send-messages bot destination response)
  (let ([tokenizer (java-new <java.util.string-tokenizer> 
                             (->jstring (filter-specials response))
                             (->jstring (string #\newline #\return)))])
    (for-each (lambda (hook) (hook destination response)) send-hooks)
    (let loop ()
      (when (->boolean (has-more-tokens tokenizer))
        (let ([tok (->string (next-token tokenizer))])
          (display (format "[to: ~a] ~a~%" destination tok))
          (send-message bot (->jstring destination) (->jstring tok))
        (loop))))))

(define join-hooks '())
(define part-hooks '())
(define action-hooks '())
(define send-hooks '())

(define (add-join-hook func)
  (set! join-hooks (cons func join-hooks)))
(define (add-part-hook func)
  (set! part-hooks (cons func part-hooks)))
(define (add-action-hook func)
  (set! action-hooks (cons func action-hooks)))
(define (add-send-hook func)
  (set! send-hooks (cons func send-hooks)))

(define (onPrivateMessage nick login host message)
  (handle-message private-channel
                  (make-irc-message #t (->string nick) (->string nick)
                                    (normalize-nick (->string nick))
                                    (->string login) (->string host)
                                    (->string message))))

(define (onMessage channel nick login host message)
  (let ([chanrec (get-channel (->string channel))])
    (handle-message chanrec 
     (make-irc-message #f (->string channel) (->string nick)
                       (normalize-nick (->symbol nick)) (->string login)
                       (->string host) (->string message)))))


(define (onPart channel sender login hostname)
  (for-each (lambda (hook)
              (apply hook (map ->string (list channel sender login hostname))))
            part-hooks))

(define (onJoin channel sender login hostname)
  (for-each (lambda (hook)
              (apply hook (map ->string (list channel sender login hostname))))
            join-hooks))

(define (onAction sender login hostname target action)
  (for-each (lambda (hook)
              (hook (get-channel (->string target)) 
                    (make-irc-message #f 
                                          (->string target) (->string
                                          sender) 
                                          (normalize-nick (->symbol sender)) 
                                          (->string login) 
                                          (->string hostname) 
                                          (->string action))))
            action-hooks))

(define (onQuit nick login hostname reason)
  (for-each (lambda (chan)
              (onPart chan nick login hostname))
            (channels-user-occupies (normalize-nick nick))))

(define (onUserList channel users)
  (let ([channel (get-channel (->string channel))])
    (for-each (lambda (nick)
                (onJoin (->jstring (channel-name channel))
                        nick nick (->jstring "unknown.net")))
              (map (lambda (arrayelem)
                     (get-nick arrayelem))
                   (->list users)))))

(define (do-join channel-name channel)
  (add-channel channel-name channel)
  (join-channel (channel-bot channel) (->jstring channel-name)))

(define (do-part channel-name)
  (let ([chan (get-channel channel-name)])
    (when chan
      (remove-channel channel-name)
      (part-channel (channel-bot chan) (->jstring channel-name)))))

(define (do-connect bot ircserver)
  (display "Connecting...\n")
  (with/fc 
   (lambda (m e)
     (print-exception (make-exception m e))
     #f)
   (lambda ()
     (connect bot (->jstring ircserver))
     (if nickserv-password
         (send-message bot (->jstring nickserv-user) 
                       (->jstring (string-append "identify " nickserv-password))))
     #t)))

(define (request-part channel message ignore term)
  (do-part term)
  "Okay.")
  
(define (handle-message chanrec message)
  (display (format (if (message-is-private? message)
                            "<from: ~a> ~a~%"
                            "[from: ~a] ~a~%")
                        (message-nick message) (message-text message)))
  (with/fc 
    (lambda (m e)
      (print-exception (make-exception m e)))
    (lambda () 
     (let loop ([handlers (channel-handlers chanrec)])
       (unless (or (null? handlers) (not ((car handlers) chanrec message)))
         (loop (cdr handlers)))))))
