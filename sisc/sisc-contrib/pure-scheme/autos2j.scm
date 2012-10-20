(require-library 'sisc/libs/srfi)

(module autos2j
  (write-autos2j-module-definition)
  (import srfi-1)
  (import srfi-8)
  (import s2j)
  (import s2j-reflection)
  (import logicops)
  (import unmangle)
  
  (define-java-classes <java.lang.reflect.field> <java.lang.reflect.method>
    <java.lang.object>)
  (define-generic-java-methods get-name get-type get-fields get-methods
    get-return-type get-parameter-types get-modifiers
    array? primitive? get-component-type)

  (define-generic-java-field-accessors (:public |PUBLIC|))

  (define (scheme-name name)
    (if (pair? name) (car name) name))
  
  (define (all-upcase? str)
    (every (lambda (c)
             (or (not (char-alphabetic? c))
                 (char-upper-case? c)))
           (string->list str)))
  
  (define (generate-module-definition modname classes field-accessors field-modifiers methods)
    `(begin (import s2j)
            (module ,modname
              (,@classes
               ,@(map scheme-name field-accessors)
               ,@(map scheme-name field-modifiers)
               ,@(map scheme-name methods))
              ,@(if (null? classes) '()
                    `((define-java-classes ,@classes)))
              ,@(if (null? field-accessors) '()
                    `((define-generic-java-field-accessors ,@field-accessors)))
              ,@(if (null? field-modifiers) '()
                    `((define-generic-java-field-modifiers ,@field-modifiers)))
              ,@(if (null? methods) '()
                    `((define-generic-java-methods ,@methods))))))
    
  (define (union ls1 ls2)
    (cond [(null? ls1) ls2]
          [(member (car ls1) ls2)

           (union (cdr ls1) ls2)]
          [(member (car ls1) (cdr ls1))
           (union (cdr ls1) ls2)]
          [else (cons (car ls1) (union (cdr ls1) ls2))]))
  
  (define (union-elem elem ls2)
    (if (member elem ls2)
        ls2
        (cons elem ls2)))

  (define (primitive-class? class)
    (or (primitive? class)
        (and (array? class)
             (primitive? (get-component-type class)))))

  (define (scan-java-classes classes)
    (let loop ([clsls classes]
               [sclasses '()] [saccessors '()] [smodifiers '()]
               [smethods '()])
      (cond [(null? clsls)
             (values sclasses saccessors smodifiers smethods)]
             ; Don't autoreflect java.lang.Object
            [(eqv? (car clsls) <java.lang.object>)
             (loop (cdr clsls) sclasses saccessors smodifiers smethods)]
            [else 
             (receive (cscl sac smod smeth) (scan-java-class (car clsls))
               (loop (cdr clsls) (union cscl sclasses) (union sac saccessors)
                     (union smod smodifiers) (union smeth smethods)))])))
  
  (define (generate-class-name t)
    (string->symbol
     (java-unmangle-class-name
      (if (java-null? (java-class-declaring-class t))
          (->string (get-name t))
          (string-append (->string
                          (get-name (java-class-declaring-class t)))
                         "$" (->string (get-name t)))))))

  (define (scan-java-class class)
    (define public
      (->number (:public (java-null
                          (java-class '|java.lang.reflect.Modifier|)))))
    (receive (sclasses saccessors smodifiers smethods)
        (scan-java-classes (java-class-declared-superclasses class))
      (for-each (lambda (jfield)
                  (when (> (logand public (->number (get-modifiers jfield))) 0)
                    (when (not (primitive-class? (get-type jfield)))
                      (set! sclasses
                            (union-elem (generate-class-name (get-type jfield))
                                        sclasses)))
                    (let* ([jfname (->string (get-name jfield))]
                           [fname  (if (all-upcase? jfname)
                                       (list (string->symbol
                                              (string-append
                                               ":"
                                               (java-unmangle-field-name
                                                (string-downcase jfname))))
                                             (string->symbol jfname))
                                       (string->symbol
                                        (string-append
                                         ":"
                                         (java-unmangle-field-name jfname))))]
                           [mname (if (pair? fname)
                                      (list (string->symbol
                                             (string-append
                                              (symbol->string (car fname))
                                              "!"))
                                            (cadr fname))
                                      (string->symbol
                                       (string-append
                                        (symbol->string fname)
                                        "!")))])
                      (set! saccessors (union-elem fname saccessors))
                      (set! smodifiers (union-elem mname smodifiers)))))
                (->list (get-fields class)))
      (for-each (lambda (jmethod)
                  (when (> (logand public
                                   (->number (get-modifiers jmethod))) 0)
                    (let ([parameters (->list (get-parameter-types jmethod))])
                      (set! sclasses (union (map generate-class-name
                                                 (filter
                                                  (lambda (v)
                                                    (not (primitive-class? v)))
                                                  (cons
                                                   (get-return-type jmethod)
                                                   parameters)))
                                            sclasses))
                      (let* ([jfname (->string (get-name jmethod))]
                             [fname (if (all-upcase? jfname)
                                        (list
                                         (string->symbol
                                          (java-unmangle-method-name
                                           (string-downcase jfname)))
                                         (string->symbol jfname))
                                        (string->symbol
                                         (java-unmangle-method-name jfname)))])
                        (set! smethods (union-elem fname smethods))))))
                (->list (get-methods class)))
      (values (union-elem (generate-class-name class) sclasses) saccessors smodifiers smethods)))
  
  (define (generate-generic-java-field-accessors fields)
    `(define-generic-field-accessors ,@(map generate-generic-accessor fields)))
  
  (define (write-autos2j-module-definition module-name classes . port)
    (receive (sclasses saccessors smodifiers smethods)
        (scan-java-classes classes)
      (parameterize ((print-shared #f))
        (apply pretty-print (generate-module-definition
                             module-name
                             sclasses
                             saccessors smodifiers smethods)
               port)))))
