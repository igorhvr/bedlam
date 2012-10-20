
(define (read-line . port)
  (let* ((char (apply read-char port)))
    (if (eof-object? char)
        char
        (do ((char char (apply read-char port))
             (clist '() (cons char clist)))
            ((or (eof-object? char) (char=? #\newline char))
             (list->string (reverse clist)))))))

(define (show-line lineno columnno file)
  (with/fc
   (lambda (m e)
     (display (format "{Could not retrieve source}~%~%")))
   (lambda ()
     (with-input-from-file file
       (lambda ()
         (let loop ([line (read-line)]
                    [l lineno])
           (if (= l 1)
               (begin
                 (display (format "[~a:~a:~a]:~%~%~a~%" 
                                  lineno columnno file line))
                 (display (make-string (- columnno 1) #\space))
                 (display #\^)
                 (newline))
               (loop (read-line) (- l 1)))))))))