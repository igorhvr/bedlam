;
; Here we define a simple remote debugging protocol.
;
; Debugger->Client 
; (<key> . <value>)
; 
; Key may be:
; state         'running 'paused 'finished 
; source-file   <string> of source file of execution position
; line-number   <number> line number of ...
; column-number <number> line number of ...
; lexicals      list of <symbol> lexical variables visible at ipoint
; breakpoints   list of <string> <number> <number> <symbol> (file, line, col, bid)
; error         <string> An error message from the debugger

;
;
; Client->Debugger
; (<command> . <argument*>)
;
; eval        <value>  Evaluate the given expression
; break       Stop at next ipoint
; set-breakpoint  (<string> <number> <number>) set breakpoint at file, line, column
; clear-breakpoint <symbol>  Clear breakpoint with given symbol
; step-into   Step to next ipoint
; step-over   Step to next ipoint at the same depth
; continue    Continue executing until next break/breakpoint
; return      <value> Apply value to the current continuation
; stop        Abort execution (will show up as an error in running code)

(import networking)
(import threading)

(define server-in)
(define server-out)
(define server-sock)

(define (accept-connection! port)
  (let* ([ss (open-tcp-listener port)]
         [sock (accept-tcp-socket ss)])
    (set! server-in (open-socket-input-port sock))
    (set! server-out (open-socket-output-port sock #t))
    (set! server-sock sock)))


(define (send-breakpoints)
  (display (format "breakpoints ~s~%" (breakpoint-list)) server-out))

(define (debugger-hook uid k oexp lvars lvacessor depth)
  (when (let ([s (getprop 'step '*debug*)])
          (or (eq? s 'into)
              (and (number? s) (= depth s))
              (breakpoint-set? uid)))
    (let-values ([(source line col) (locate-ipoint uid)])
      (when source
        (display (format "source-file ~s~%line-number ~a~%column-number ~a~%"
                         source line col)
                 server-out)))
    (display (format "lexicals ~s~%" lvars) server-out)
    (display "state paused\n" server-out)
    (handle-command depth)))

(trace 'read)

(define (handle-command depth)
  (let ([command (read server-in)])
    (case command
      ((eval set-breakpoint clear-breakpoint return)
       (let ([arg (read server-in)])
         (case command
           ((eval) 
            (display "state running\n" server-out)
            (display (format "result ~s~%" (eval arg)) server-out)
            (display "state stopped\n" server-out))
           ((clear-breakpoint) 
            (clear-breakpoint! arg)
            (send-breakpoints)
            (handle-command depth))
           ((set-breakpoint) 
            (let-values ([(source line column)
                          (apply values arg)])
              (set-file-breakpoint! source line column)
              (send-breakpoints)
              (handle-command depth)))
           ((return) #f)
           (else (display (format "error \"unexpected command '~a'\~%" command)
                          server-out)))))
      ((break) #f)
      ((step-into) (putprop 'step '*debug* 'into) )
      ((step-over) (putprop 'step '*debug* depth))
      ((continue) (void))
      ((stop) #f)
      (else (display (format "error \"unexpected command '~a'\~%" command)
                     server-out)
            (handle-command depth)))))

(define (start-remote-debugger . port)
  (let ([port (if (null? port) 20000 (car port))])
    (thread/start
     (thread/new (lambda ()
                   (accept-connection! port)
                   (send-breakpoints)
                   (display "state stopped\n" server-out)
                   (let loop ()
                     (handle-command 0)
                     (loop)))))))
  