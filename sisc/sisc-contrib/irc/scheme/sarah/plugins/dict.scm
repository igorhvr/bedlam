(define (query-dictionary database word)
  (import srfi-13)
  (let* ([sock (open-tcp-socket "dict.org" 2628)]
         [out (open-socket-output-port sock #t)]
         [def
          (with-output-to-string
            (lambda ()
              (with-input-from-port (open-socket-input-port sock)
                (lambda ()
                  (when (= 220 (read))
                    (read-line)
                    (display (format "DEFINE ~a ~a\r\n"
                                          database word) out)
                    (let ([rc (read)])
                      (cond [(> 200 rc 100)
                             (let ([c (read)])
                               (read-line)
                               (do [(count c (- count 1))]
                                   [(zero? count)]
                                 (read-line)
                                 (do ([r (read-line) (read-line)])
                                     ((equal? (string-trim-both r) "."))
                                   (display (string-trim-both r))
                                   (newline))))]
                            [(= rc 552)
                             (display "Sorry, I couldn't find that word.")]
                            [else
                             (display
                              (format
                               "Error ~a retriving dictionary entry for '~a'"
                               rc word))])))))))])
    (close-output-port out)
    (close-socket sock)
    def))
              
(define (dict source)
  (lambda (channel message ignore term)
    (query-dictionary source term)))
    