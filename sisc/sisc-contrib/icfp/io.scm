; IO Routines

(define (connect host port)
  (let* ((sock (open-tcp-socket host port))
	 (in (open-socket-input-port sock))
	 (out (open-socket-output-port sock)))
    (list sock in out)))

(define (read-line . port)
  (let* ((char (apply read-char port)))
    (if (eof-object? char)
	char
	(do ((char char (apply read-char port))
	     (clist '() (cons char clist)))
	    ((or (eof-object? char) (char=? #\newline char))
	     (list->string (reverse clist)))))))

(define (check-newline in)
  (let ((next-char (read-char in)))
    (if (not (eq? next-char #\newline))
	(error 'receive-world 
	       "Expected a newline, got '~a'." next-char))))

(define (receive-world in)
  (let* ((w (read in))
	 (h (read in))
	 (world (make-world! w h)))
    (check-newline in)
    (do ((y 1 (+ y 1)))
	((> y h) world)
      (do ((x 1 (+ x 1)))
	  ((> x w))
	(let ((square (read-char in)))
	  (world-set! world x y square)
	  (if (eq? square BASE)
	      (add-home-base! x y))))
      (check-newline in))))

  
(define (receive-configuration in)
  (let* ((id (read in))
	 (cap (read in))
	 (money (read in)))
    (check-newline in)
    (debug "Setting my cap: ~a" cap)
    (robot-capacity! id cap)
    (robot-money! id money)
    id))

(define (receive-packages in rid)
  (let ((ln (read-line in)))
    (let loop ((ls (read (open-input-string (string-append "(" ln ")")))))
      (unless (null? ls) 
        (let ((id (car ls))
              (x (cadr ls))
              (y (caddr ls))
              (weight (cadddr ls)))
          (let ((p (package-lookup id)))
            (package-details! p x y weight)
            (apply package-location! (cons p (robot-position rid)))
            (package-add! p)
            (loop (cddddr ls))))))))
        
(define (more-packages? in)
  (let ((nc (read-char in)))
    (and (not (eof-object? nc))
	 (not (eq? nc #\newline)))))

(define more-responses? more-packages?)
(define more-robots? more-packages?)

(define (send-command out bid command . args)
  (display (format "~a ~a" bid command) out)
  (for-each (lambda (arg)
	      (display #\space out)
	      (display arg out))
	    args)
  (newline out)
  (flush-output-port out))


(define (receive-responses id in)
  (let ((ln (read-line in)))
    (debug "server response: ~a" ln)
    (call-with-input-string ln
      (lambda (in)
        (let loop () 
          (unless (eof-object? (read-char in))
            (let ((id (read in)))
              (let loop2 () 
                (unless (eof-object? (read-char in))
                  (if (eq? #\# (peek-char in))
                      (loop)
                      (let ((command (read in)))
                        (parse-response id command in)
                        (loop2))))))))))))

(define (receive-gamestate in)
  (receive-world in)
  (receive-configuration in))

(define (handshake in out)
  (display (format 
            ;"Login 12 1~%" 
            "Player~%") out)
  (flush-output-port out))

;(trace 'receive-configuration 'read-char 'receive-responses 'parse-response)
