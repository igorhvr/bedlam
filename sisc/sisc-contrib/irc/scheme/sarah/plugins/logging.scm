(define (chanlog channel message ignore state)
  (cond [(equal? state "on")
         (unless (channel-log-file channel)
           (set-channel-log-file! channel
             (open-output-file
                (let loop ([i 0])
                  (let ([fname 
                           (format "/arc/scratch/sarah-logs/~a_~a_~a.log"
                               (substring (channel-name channel) 1
                                          (string-length 
                                                  (channel-name channel)))
                               (date->string (current-date) "~Y-~m-~d") i)])
                    (if (file-exists? fname)
                        (loop (+ i 1))
                        fname))) #t)))
          "Okay"]
        [(equal? state "off")
         (let ([oldlog (channel-log-file channeL)])
           (if oldlog (close-output-port oldlog))
           (set-channel-log-file! channel #f))
         "Okay."]
        [else 'continue]))
