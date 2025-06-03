(import custom-io)
(import string-io) ;; For call-with-output-string in tests
(import s2j)

;; create-unary-function-based-output-port receives a unary function and returns a
;; custom character output port. When data is written to this port,
;; the unary function is called with that data.
(define (create-unary-function-based-output-port unary-fn)
  ;; Port state: vector containing #(unary-fn is-closed?)
  (let ((port-state (vector unary-fn #f)))

    (define (write-char-proc port char-code)
      (if (vector-ref (port-local port) 1) ;; is-closed?
          (error 'write-char-proc "Port is closed")
          (begin
            (let ((fn (vector-ref (port-local port) 0)))
              (fn char-code))
            (void))))

    (define (write-string-proc port str offset count)
      (if (vector-ref (port-local port) 1) ;; is-closed?
          (error 'write-string-proc "Port is closed")
          (begin
            (let ((fn (vector-ref (port-local port) 0)))
              (fn (substring str offset (+ offset count))))
            (void))))

    (define (flush-proc port)
      (if (vector-ref (port-local port) 1) ;; is-closed?
          (error 'flush-proc "Port is closed")
          (void)))

    (define (close-proc port)
      (vector-set! (port-local port) 1 #t) ;; Set is-closed? to true
      (void))

    (let ((custom-port (make-custom-character-output-port write-char-proc write-string-proc flush-proc close-proc)))
      (set-port-local! custom-port port-state)
      custom-port)))

(define (test-create-unary-function-based-output-port)
  (display "Running tests for create-unary-function-based-output-port...\n")
  (let* ((collected-data '())
         (collecting-fn (lambda (s) (set! collected-data (cons s collected-data))))
         (port (create-unary-function-based-output-port collecting-fn)))

    (display "Test U1.1: write-char #\\A\n")
    (write-char #\A port)
    (d/n (reverse collected-data))
    (assert (equal? (car collected-data) #\A))
    (set! collected-data '())

    (display "Test U1.2: display \"Hi\"\n")
    (display "Hi" port)
    (d/n (reverse collected-data))
    (set! collected-data '())

    (display "Test U1.3: write-string \"Test\" 0 4\n")
    (write-string "Test" 0 4 port)
    (d/n (reverse collected-data))
    (set! collected-data '())

    (display "Test U1.4: write-string \"Substring\" 3 6\n")
    (write-string "Substring" 3 6 port) ;; "string"
    (d/n (reverse collected-data))
    (set! collected-data '())

    (display "Test U1.5: display 123\n")
    (display 123 port) ;; display calls write-char for '1', '2', '3'
    (d/n (reverse collected-data))
    (set! collected-data '())

    (display "Test U1.6: write 'my-symbol\n")
    (write 'my-symbol port) ;; write calls write-char for 'm', 'y', ...
    (d/n (reverse collected-data))
    (set! collected-data '())

    (close-port port)
    (display "Test U1.7: write-char after close (should error)\n")
    (let ((error-caught #f))
      (with-failure-continuation
       (lambda (err k)
         (set! error-caught #t))
       (lambda ()
         (write-char #\X port)
         (display "FAIL - write-char on closed port did not error.\n")))
      (assert error-caught))

    (display "Test U1.8: write-string after close (should error)\n")
    (let ((error-caught #f))
      (with-failure-continuation
       (lambda (err k)
         (set! error-caught #t))
       (lambda ()
         (write-string "Closed" 0 6 port)
         (display "FAIL - write-string on closed port did not error.\n")))
      (assert error-caught))

    (display "Test U1.9: flush after close (should error)\n")
    (let ((error-caught #f))
      (with-failure-continuation
       (lambda (err k)
         (set! error-caught #t))
       (lambda ()
         (flush-output-port port)
         (display "FAIL - flush on closed port did not error.\n")))
      (assert error-caught))

    (display "create-unary-function-based-output-port tests passed.\n")))

;; create-thunk-based-input-port receives a thunk and returns a custom input port that
;; retrieves information from a thunk that it will call repeatedly as needed. The thunk
;; always returns a string or #f if there is no more data to serve right now. 
(define (create-thunk-based-input-port thunk)
  ;; Port state: vector containing #(current-string-buffer current-index thunk-procedure)
  ;; current-string-buffer: The string from the last thunk call, or #f if empty/exhausted.
  ;; current-index: The next character position to read from current-string-buffer.
  ;; thunk-procedure: The user-provided thunk, or #f if it has signaled permanent EOF.
  (let ((port-state (vector #f 0 thunk)))

    (define (read-char-proc port)
      (let loop-read-char () ;; Loop to retry thunk if it returns an empty string
        (let* ((buffer (vector-ref port-state 0))
               (index (vector-ref port-state 1))
               (current-thunk (vector-ref port-state 2)))
          (if (not current-thunk) ;; Thunk permanently exhausted
              -1 ;; EOF
              (if (or (not buffer) (>= index (string-length buffer)))
                  ;; Buffer empty or exhausted, try to fill it
                  (let ((new-data (current-thunk)))
                    (cond
                     ((string? new-data)
                      (vector-set! port-state 0 new-data) ;; Store new data
                      (vector-set! port-state 1 0)     ;; Reset index
                      (if (string=? new-data "")
                          (begin
                            (vector-set! port-state 0 #f) ;; Ensure buffer is #f for next check
                            (loop-read-char)) ;; Empty string, try thunk again
                          (loop-read-char))) ;; Got non-empty string, re-process to read char
                     ((not new-data) ;; Thunk returned #f: permanent EOF
                      (vector-set! port-state 0 #f)
                      (vector-set! port-state 2 #f) ;; Mark thunk as exhausted
                      -1) ;; EOF
                     (else ;; Should not happen based on thunk contract
                      (error "Thunk returned invalid data:" new-data))))
                  ;; Buffer has data, read next char
                  (let ((char-to-return (string-ref buffer index)))
                    (vector-set! port-state 1 (+ index 1))
                    (char->integer char-to-return)))))))

    (define (read-string-proc port mutable-string offset count)
      (let ((chars-read 0))
        (let loop ()
          (if (< chars-read count)
              (let* ((buffer (vector-ref port-state 0))
                     (index (vector-ref port-state 1))
                     (current-thunk (vector-ref port-state 2)))
                (cond
                 ((not current-thunk) ;; Thunk permanently exhausted
                  chars-read) ;; Return whatever was read before exhaustion
                 ((or (not buffer) (>= index (string-length buffer)))
                  ;; Buffer empty or exhausted, try to fill it
                  (let ((new-data (current-thunk)))
                    (cond
                     ((string? new-data)
                      (vector-set! port-state 0 new-data) ;; Store new data (even if empty)
                      (vector-set! port-state 1 0)     ;; Reset index
                      (if (string=? new-data "")
                          (vector-set! port-state 0 #f)) ;; Clear buffer if empty, so next loop re-calls thunk
                      (loop)) ;; Continue by re-entering main loop / processing new data
                     ((not new-data) ;; Thunk returned #f: permanent EOF
                      (vector-set! port-state 0 #f)
                      (vector-set! port-state 2 #f) ;; Mark thunk as exhausted
                      chars-read)
                     (else
                      (error "Thunk returned invalid data:" new-data)))))
                 (else
                  ;; Buffer has data, copy to mutable-string
                  (let* ((available-in-buffer (- (string-length buffer) index))
                         (chars-to-copy-this-round (min available-in-buffer (- count chars-read))))
                    (do ((i 0 (+ i 1)))
                        ((>= i chars-to-copy-this-round))
                      (string-set! mutable-string
                                   (+ offset chars-read i)
                                   (string-ref buffer (+ index i))))
                    (set! chars-read (+ chars-read chars-to-copy-this-round))
                    (vector-set! port-state 1 (+ index chars-to-copy-this-round))
                    (loop))))))) ;; Continue if more chars needed
        (if (and (= chars-read 0)
                 (not (vector-ref port-state 2)) ; thunk exhausted
                 (let ((buffer (vector-ref port-state 0))) (or (not buffer) (string=? buffer "")))) ; and buffer is effectively empty
            -1 ;; EOF before any characters were read
            chars-read)))

    (define (ready?-proc port)
      (let* ((buffer (vector-ref port-state 0))
             (index (vector-ref port-state 1))
             (current-thunk (vector-ref port-state 2)))
        (cond
         ((and buffer (< index (string-length buffer))) #t) ;; Data in current buffer
         ((not current-thunk) #f) ;; Thunk permanently exhausted
         (else ;; Try to prefetch
          (let ((new-data (current-thunk)))
            (cond
             ((string? new-data)
              (if (string=? new-data "")
                  #f ;; Thunk returned empty string, not ready now but might be later
                  (begin ;; Got data
                    (vector-set! port-state 0 new-data)
                    (vector-set! port-state 1 0)
                    #t)))
             ((not new-data) ;; Thunk returned #f: permanent EOF
              (vector-set! port-state 0 #f)
              (vector-set! port-state 2 #f)
              #f)
             (else
              (error "Thunk returned invalid data in ready?:" new-data))))))))

    (define (close-proc port)
      (vector-set! port-state 0 #f)
      (vector-set! port-state 1 0)
      (vector-set! port-state 2 #f) ;; Mark thunk as unusable
      (void))

    (let ((custom-port (make-custom-character-input-port read-char-proc read-string-proc ready?-proc close-proc)))
      (set-port-local! custom-port port-state)
      custom-port)))

(define (test-create-thunk-based-input-port)
  (display "Running tests for create-thunk-based-input-port...\n")

  ;; Test 1: Basic read-char
  (let* ((data-chunks (list "He" "llo" "" " " "W" "orld" "!" #f "extra"))
         (thunk (lambda ()
                  (if (null? data-chunks)
                      #f
                      (let ((chunk (car data-chunks)))
                        (set! data-chunks (cdr data-chunks))
                        chunk))))
         (port (create-thunk-based-input-port thunk)))
    (let ((result (call-with-output-string
                   (lambda (sop)
                     (let loop ()
                       (let ((char-val (read-char port)))
                         (unless (eof-object? char-val)
                           (write-char char-val sop)
                           (loop))))))))
      (display "Test 1 (read-char) result: \"") (display result) (display "\"\n")
      (assert (string=? result "Hello World!"))))

  ;; Test 2: read-string
  (let* ((data-chunks (list "abc" "def" "ghi" #f))
         (thunk (lambda ()
                  (if (null? data-chunks)
                      #f
                      (let ((chunk (car data-chunks)))
                        (set! data-chunks (cdr data-chunks))
                        chunk))))
         (port (create-thunk-based-input-port thunk))
         (buf (make-string 10 #\?)))
    (let ((count1 (read-string buf 0 4 port))) ;; "abcd"
      (display "Test 2.1 (read-string 4) count: ") (display count1) (display ", string: \"") (display (substring buf 0 count1)) (display "\"\n")
      (assert (= count1 4))
      (assert (string=? (substring buf 0 4) "abcd")))
    (let ((count2 (read-string buf 0 4 port))) ;; "efgh"
      (display "Test 2.2 (read-string 4) count: ") (display count2) (display ", string: \"") (display (substring buf 0 count2)) (display "\"\n")
      (assert (= count2 4))
      (assert (string=? (substring buf 0 4) "efgh")))
    (let ((count3 (read-string buf 0 4 port))) ;; "i"
      (display "Test 2.3 (read-string 4) count: ") (display count3) (display ", string: \"") (display (substring buf 0 count3)) (display "\"\n")
      (assert (= count3 1))
      (assert (string=? (substring buf 0 1) "i")))
    (let ((count4 (read-string buf 0 4 port))) ;; EOF
      (display "Test 2.4 (read-string 4) count: ") (display count4) (display "\n")
      (assert (eof-object? count4))))

  ;; Test 3: ready?
  (let* ((data-chunks (list "a" "" "b" #f))
         (thunk (lambda ()
                  (if (null? data-chunks)
                      #f
                      (let ((chunk (car data-chunks)))
                        (set! data-chunks (cdr data-chunks))
                        chunk))))
         (port (create-thunk-based-input-port thunk)))
    (display "Test 3.1 (ready? initial): ") (display (char-ready? port)) (newline)
    (assert (char-ready? port))                 ;; Prefetches "a"
    (assert (char=? (read-char port) #\a))

    (display "Test 3.2 (ready? after reading 'a', thunk returns \"\"): ") ;(display (char-ready? port)) (newline) (display (char-ready? port)) (newline)
    (assert (not (char-ready? port)))           ;; Thunk returns "", so not ready now

    (display "Test 3.3 (ready? after thunk returned \"\", now returns 'b'): ") (display (char-ready? port)) (newline)
    (assert (char-ready? port))                 ;; Thunk returns "b"
    (assert (char=? (read-char port) #\b))

    (display "Test 3.4 (ready? after reading 'b', thunk returns #f): ") (display (char-ready? port)) (newline)
    (assert (not (char-ready? port)))           ;; Thunk returns #f
    (assert (eof-object? (read-char port))))

  ;; Test 4: Close port
    (let* ((data-chunks (list "abc" #f))
         (thunk-called #f)
         (thunk (lambda ()
                  (set! thunk-called #t)
                  (if (null? data-chunks)
                      #f
                      (let ((chunk (car data-chunks)))
                        (set! data-chunks (cdr data-chunks))
                        chunk))))
         (port (create-thunk-based-input-port thunk)))
    (close-port port)
    (set! thunk-called #f) ;; Reset flag
    (display "Test 4.1 (ready? on closed port): ") (display (char-ready? port)) (newline)
    (assert (not (char-ready? port)))
    (assert (not thunk-called)) ;; Thunk should not be called on closed port by ready?

    (display "Test 4.2 (read-char on closed port expects IOException): ")
    (let ((ioexception-was-caught #f))
      (with-failure-continuation
       (lambda (error-record error-continuation)
         (let ((jex-pair (assoc 'java-exception error-record)))
           (if (and jex-pair (instance-of (cdr jex-pair) "java.io.IOException"))
               (begin
                 (set! ioexception-was-caught #t)
                 (display "IOException caught as expected.") (newline))
               (begin ;; Some other error, re-throw it to fail the test clearly
                 (display "Unexpected error caught: ")
                 ;; (print-exception (make-exception error-record error-continuation)) (newline) ;; SISC already prints it
                 (throw (make-exception error-record error-continuation))))))
       (lambda ()
         (read-char port) ;; This call should trigger the failure continuation
         ;; If we reach here, read-char did not throw an error, which is a test failure.
         (display "FAIL - read-char on closed port did not throw an exception.") (newline)
         (set! ioexception-was-caught #f))) ;; Explicitly mark as failed
      (assert ioexception-was-caught))
    (assert (not thunk-called))) ;; Thunk should not be called on closed port by read-char

  (display "create-thunk-based-input-port tests passed.\n"))

