(module iasylum/macros
  (code->macroexpanded-code file->macroexpanded-code)

  ;;; FIXXXME Will fail badly in many cases. Handling if namespaces is irresponsible to say the least, but it is still useful in some cases.
  (define code->macroexpanded-code
    (lambda* (code (warn-me-that-this-is-really-unsafe #t))
             (when warn-me-that-this-is-really-unsafe
               (log-warn "code->macroexpanded-code called. It will fail badly (and silently, conceivably) in many cases. Handling of namespaces is irresponsible to say the least, but it is still useful in limited scenarios. Consider yourself warned, and here be dragons!"))
             
             (let ((previous-prefixing (vector-length-prefixing #f)))
               (and-let* ((dirty-code (iasylum-write-string (caddr (sc-expand code))))
                          (bar-code (irregex-replace/all "#%" dirty-code ""))
                          (namespaced-code (irregex-replace/all '(seq "|")  bar-code ""))
                          (result-with-invalid-empty-list
                           (irregex-replace/all '(seq "@" (+ (- any ":")) "::")  namespaced-code ""))
                          
                          (step-1  (irregex-replace/all
                                    '(seq "(lambda ()")
                                    result-with-invalid-empty-list "e68e8d18-13be-4b0d-8cac-6a0045cd99df"))
                          (step-2  (irregex-replace/all '(seq "()")
                                                        step-1 "(list)"))
                          (step-3  (irregex-replace/all '(seq "e68e8d18-13be-4b0d-8cac-6a0045cd99df")
                                                        step-2 "(lambda ()"))
                          (step-4  (irregex-replace/all '(seq "(lambda #t")
                                                        step-3 "(lambda ()"))
                          (step-5  (irregex-replace/all '(seq whitespace "#(")
                                                        step-4 " '#("))
                          (final-result  (with-input-from-string step-5 (lambda () (read)))))
                 (begin
                   (vector-length-prefixing previous-prefixing)
                   final-result)))))

  (define (fix-empty-lists str)
    (let* ((step-1 (irregex-replace/all '(seq "'()") str "(empty-list)"))
           (step-2 (irregex-replace/all '(seq "(quote ())") str "(empty-list)")))
      step-2))
  
  (define (file->macroexpanded-code fname)
    (let ((cd (with-input-from-string
                  (fix-empty-lists (file->string fname))
                (lambda () (let loop ()
                        (let ((obj (read)))
                          (if (eof-object? obj) '()
                              (cons obj (loop)))))))))
      (map code->macroexpanded-code cd)))
  
  )