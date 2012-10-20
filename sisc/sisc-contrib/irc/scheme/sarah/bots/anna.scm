(define-java-class <java.net.url-encoder> |java.net.URLEncoder|)
(define-generic-java-method encode)
	          
(define anna-mutex (mutex/new))                              

(define ask-anna
  (let ([last-user #f])
    (lambda (from message)
      (mutex/synchronize-unsafe anna-mutex
       (lambda ()
	 (unless (equal? from last-user)
  	   (call-with-input-file 
	       (format "http://localhost:2001/?text=Call+me+~a." from)
	     read-line)
	   (set! last-user from))
	 (call-with-input-file 
	     (format "http://localhost:2001/?text=~a"
			  (->string (encode (java-null <java.net.url-encoder>)
					    (->jstring message))))
	   read-line))))))

(define (annabot channel message)
  (let ([response (ask-anna (message-nick message)
                            (bot-clean (->string (get-name (channel-bot channel)))
                                       (message-text message)))])
    (display "From anna: ")
    (send-messages (channel-bot channel)
                   (message-source message) response)
    #t))