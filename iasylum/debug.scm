; Code from tip by Daniel Sadilek <sadilek@informatik.hu-berlin.de>

(module iasylum/debug
  (debug
   iasylum-debug-nothing-continuation) ; TODO Figure out why exporting the iasylum-debug-nothing-continuation is required

  (define iasylum-debug-nothing-continuation)
  
  (define-syntax debug
    (syntax-rules ()
      ((_ var ...)
       (begin
         (for-each display (list "\n\nSTART: DEBUGGING MODE\n\n  (exitd) to simply exit.\n  (stopd) to exit and also get out of current context\n\nAvailable lexical bindings: " '(var) ...))
         (display "\n")
         (let ((child-env (make-child-environment (interaction-environment)))
               (force-stop (box #f)))
           (putprop 'var child-env var) ...
           (putprop 'stopd child-env
                    (lambda ()
                      (set-box! force-stop #t)
                      (exit)))
           (putprop 'exitd child-env
                    (lambda () (exit)))
           (with-environment child-env (lambda ()
                                         (repl)))
           (display "\n\nEND: DEBUGGING MODE (")
           (display (if (unbox force-stop) "stopd" "exitd"))
           (display ")\n\n")
           (if (unbox force-stop) (iasylum-debug-nothing-continuation))
           )))))

  (call/cc (lambda (c) (set! iasylum-debug-nothing-continuation c)))
  )