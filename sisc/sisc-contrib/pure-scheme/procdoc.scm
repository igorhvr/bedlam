; A module utilizing procedure-properties to allow online help for
; arbitrary procedures

  
(module procedure-selfdoc
    ((document-procedure) help)
  (import procedure-properties)
  (define-syntax document-procedure 
    (syntax-rules (args:)
      ((_ proc 
          (args: arguments ...)
          (return-type: rt)
          helplines ...)
       (begin 
         (set-procedure-property! proc 'help
           (string-append (format "autodoc: ~a => ~a~%~%" 
                                  (quote (proc arguments ...))
                                  (quote rt))
                          helplines ...))
         (void)))
      ((_ proc (args: arguments ...) helplines ...)
       (begin 
         (set-procedure-property! proc 'help
           (string-append (format "autodoc: ~a~%~%" 
                                  (quote (proc arguments ...)))
                          helplines ...))
         (void)))
      ((_ proc (return-type: rt) helplines ...)
       (begin 
         (set-procedure-property! proc 'help
           (string-append (format "autodoc: ~a => ~a~%~%" 
                                  (quote proc)
                                  (quote rt))
                          helplines ...))
         (void)))
      ((_ proc helplines ...)
       (begin
         (set-procedure-property! proc 'help
           (string-append (format "autodoc: ~a~%~%" (quote proc))
                          helplines ...))
         (void)))))
  (define (help proc)
    (let ([prop (procedure-property proc 'help)])
      (display
       (if prop prop "{No help available}"))
      (newline))))
