(module iasylum/sparse/sparse-vectors/sparse-vectors-utils
  (arithmetic-shift define-record)
  
  (define arithmetic-shift ashl)

  (define-syntax define-record
    (lambda (x)    
      (syntax-case x ()
        ((_ name slot-name) ;;TODO: Make this work with multiple slots.
         (let* ((symbolic-name (syntax-object->datum (syntax name)))
                (record-symbolic-name (string->symbol (string-append "record-" (symbol->string symbolic-name))))
                (slot-v (syntax-object->datum (syntax slot-name)))
                (slot-v-s (symbol->string slot-v))
                (string-name (symbol->string symbolic-name))
                (make-name (string->symbol (string-append "make-" string-name)))
                (predicate-name (string->symbol (string-append string-name "?")))
                (getter-name (string->symbol (string-append string-name "-" slot-v-s)))
                (setter-name (string->symbol (string-append string-name "-" slot-v-s "-set!"))))
           (with-syntax ((record-symbolic-name (datum->syntax-object (syntax _) record-symbolic-name))
                         (slot-name (datum->syntax-object (syntax _) slot-v))
                         (string-name (datum->syntax-object (syntax _) string-name))
                         (make-name (datum->syntax-object (syntax _) make-name))
                         (predicate-name (datum->syntax-object (syntax _) predicate-name))
                         (getter-name (datum->syntax-object (syntax _) getter-name))
                         (setter-name (datum->syntax-object (syntax _) setter-name)))
             (syntax
              (begin
                (define-record-type record-symbolic-name
                  (make-name slot-name)
                  predicate-name
                  (slot-name getter-name setter-name))))))))))
  )