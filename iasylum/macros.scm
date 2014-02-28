(module iasylum/macros
  (code->macroexpanded-code file->macroexpanded-code)

  ;;; FIXXXME Will fail badly in many cases. Handling if namespaces is irresponsible to say the least, but it is still useful in some cases.
  (define* (code->macroexpanded-code code (warn-me-that-this-is-really-unsafe #t))
    (when warn-me-that-this-is-really-unsafe
      (log-warn "code->macroexpanded-code called. It will fail badly (and silently, conceivably) in many cases. Handling if namespaces is irresponsible to say the least, but it is still useful in limited scenarios. Consider yourself warned, and here be dragons!"))
    
    (let ((prepared-code-string 
           (let ((dirty-code (iasylum-write-string
                              (caddr (sc-expand code)))))                                                
             (let ((bar-code (irregex-replace/all "#%" dirty-code "")))
               (let ((namespaced-code (irregex-replace/all '(seq "|")  bar-code "")))
                 (let ((result (irregex-replace/all '(seq "@" (+ (- any ":")) "::")  namespaced-code "")))
                   result))))))
      (with-input-from-string prepared-code-string (lambda ()
                                                     (read)))))
  
  (define (file->macroexpanded-code fname)
    (let ((cd (with-input-from-string
                  (file->string fname)
                (lambda () (let loop ()
                        (let ((obj (read)))
                          (if (eof-object? obj) '()
                              (cons obj (loop)))))))))
      (map code->macroexpanded-code cd)))
  
  )