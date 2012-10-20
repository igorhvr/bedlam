(require-library 'sisc/libs/srfi)

(module unmangle
  (java-unmangle-field-name java-unmangle-method-name
                            java-unmangle-class-name)
  (import srfi-13)
  (import srfi-14)
  (import s2j)
  (import s2j-reflection)
  
  (define (java-unmangle-field-name str)
    (let loop ([upcase (and (> (string-length str) 0)
                            (char-upper-case? (string-ref str 0)))]
               [strl (string->list str)]
               [acc '()])
      (cond [(null? strl)
             (list->string (reverse acc))]
            [(char-upper-case? (car strl))
             (loop #t (cdr strl) (cons (char-downcase (car strl))
                                       (if (null? acc) acc
                                           (cons #\- acc))))]
            ; The following two don't really relate to fields, but it
            ; makes life easier for the other functions
            [(char=? (car strl) #\;)
             (loop #f (cdr strl) acc)]
            [(char=? (car strl) #\$)
             (string-append (list->string (reverse acc))
                            "/" (loop #f (cdr strl) '()))]
            [else (loop (char-upper-case? (car strl))
                        (cdr strl)
                        (cons (car strl) acc))])))
  
  (define (java-unmangle-method-name str)
    (let* ([len (string-length str)]
           [append-q (and (> len 3)
                          (string-prefix? "is" str)
                          (char-upper-case? (string-ref str 2)))]
           [unmangled-field (java-unmangle-field-name str)])
      (if append-q
          (string-append (string-drop unmangled-field 3) "?")
          unmangled-field)))
  
  (define (tokenize-between-dots str)
    (let loop ([strl (string->list str)]
               [wordacc '()])
      (cond [(null? strl) 
             (if (null? wordacc) '()
                 (list (list->string (reverse wordacc))))]
            [(char=? (car strl) #\.)
             (cons (list->string (reverse wordacc))
                   (loop (cdr strl) '()))]
            [else (loop (cdr strl)
                        (cons (car strl) wordacc))])))

  (define (java-unmangle-class-name str)
    (define (finish-class-mangling str acc array-dim)
      (string-append
       "<"
       (apply string-append
              (let loop ([tokens (tokenize-between-dots str)])
                ;perform field unmangling between the dots
                (if (null? tokens) '()
                    (let ([rest (loop (cdr tokens))])
                      (cons (java-unmangle-field-name (car tokens))
                            (if (null? rest) '()
                                (cons "." rest)))))))
       (if (> array-dim 0)
           (apply string-append
                  (append (map (lambda (x) "[]")
                               (iota array-dim))
                          '(">")))
           ">")))
    (let loop ([strl (string->list str)]
               [acc '()]
               [array-dim 0])
      (cond [(null? strl) (finish-class-mangling
                           (list->string strl)
                           acc array-dim)]
            [(char=? (car strl) #\[)
             (cond [(or (null? (cdr strl))
                        (char=? (cadr strl) #\[))
                    (loop (cdr strl) acc (+ array-dim 1))]
                   [(char=? (cadr strl) #\L)
                    ;Handle non-primitive array types
                    (loop (cddr strl) acc (+ array-dim 1))]
                   [else (error 'java-unmangle-class-name
                                "cannot unmangle primitive array types.")])]
            [else (finish-class-mangling
                   (list->string strl)
                   acc array-dim)]))))
        
