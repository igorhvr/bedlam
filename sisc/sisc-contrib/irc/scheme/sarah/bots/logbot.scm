(define (logbot channel message)
  (do-log channel
          (format "<~a> ~a" 
                       (message-nick message) 
                       (message-text message))))

(define (do-log channel message-text)
   (if (channel-log-file channel)
       (display (format "[~a] ~a~%" 
                             (date->string (current-date) "~H:~M")
                             (word-wrap message-text "    "))
                (channel-log-file channel))))

(define (word-wrap text line-prefix)
    (if (<= (string-length text) 70)
        text
        (let ([tokenizer (java-new <java.util.string-tokenizer> 
                                   (->jstring (filter-specials text)))])
          (with-output-to-string
            (lambda ()
              (let loop ([len 0])
                (when (->boolean (has-more-tokens tokenizer))
                  (let ([tok (->string (next-token tokenizer))])
                    (if (> (+ 1 len (string-length tok)) 70)
                        (begin
                          (display #\newline)
                          (display line-prefix)
                          (display tok)
                          (loop 1))
                        (begin
                          (when (> len 0)
                            (display #\space))
                          (display tok)
                          (loop (+ 1 (string-length tok) len))))))))))))
      
(add-action-hook
 (lambda (channel message)
   (do-log channel (format "* ~a ~a" (message-nick message)
                                          (message-text message)))))

(add-join-hook
 (lambda (channel sender login hostname)
   (do-log (get-channel channel) (format "~~ ~a has joined." sender))))

(add-part-hook
 (lambda (channel sender login hostname)
   (do-log (get-channel channel) (format "~~ ~a has left." sender))))

(add-send-hook
 (lambda (target message)
   (let ([chan (get-channel target)])
     (when chan
       (logbot chan (make-irc-message #f chan bot-name #f #f #f message))))))