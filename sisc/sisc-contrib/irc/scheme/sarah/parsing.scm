(define-generic-java-method jtrim trim)
(define <jstring> (java-class '|java.lang.String|))

(define (normalize-nick nick)
  (cond [(instance-of? nick <jstring>)
         (metaphone (->string nick))]
        [(symbol? nick)
         (metaphone (symbol->string nick))]
        [(string? nick)
         nick]
        [else (error 'normalize-nick "Invalid nick type ~a" nick)]))

(define somewhat-clean
  (string->list "abcdefghijklmnopqrstuvwxyz()[]->+-*/_01234567890?!."))
(define really-clean
  (string->list "abcdefghijklmnopqrstuvwxyz0123456789"))

(define (tokens->string tokens)
  (let ([str
         (with-output-to-string
             (lambda ()
               (for-each (lambda (tok)
                           (display tok)
                           (display #\space))
                         tokens)))])
    (substring str 0 (- (string-length str) 1))))


(define (trim str)
 (->string (jtrim (->jstring str))))

(define (crib-split on str)
  (import srfi-13)
  (if (eq? on #t)
      (list "" (trim str))
      (let ([i (string-contains (string-downcase str) on)])
        (if i
            (list (trim (substring str 0 i))
                  (trim (substring str (+ i (string-length on)) (string-length str))))
            (list #f #f)))))

(define (clean word charset)
  (if (null? word) 
      '()
      (let ((c (char-downcase (car word))))
        (if (memv c charset)
            (cons c (clean (cdr word) charset))
            (clean (cdr word) charset)))))

(define (tokenize m) (tokenize-ignoring-punct (string->list m) '()))


(define (tokenize-ignoring-punct ls acc)
  (cond [(null? ls) (or (and (not (null? acc))
                             (list (string->symbol  
                                    (list->string (clean (reverse acc) somewhat-clean)))))
                        '())]
        [(memv (car ls) '(#\newline #\tab #\space))
         (if (not (null? acc))
             (cons (string->symbol
                    (list->string (clean (reverse acc) somewhat-clean)))
                    (tokenize-ignoring-punct (cdr ls) '()))
             (tokenize-ignoring-punct (cdr ls) acc))]
        [else (tokenize-ignoring-punct (cdr ls) (cons (car ls) acc))]))

(define (bot-clean bot-name message)
  (let ([bot-name-length (string-length bot-name)])
    (trim 
     (let loop ([x 0])
       (cond [(or (= x (string-length message))
                  (> x (+ bot-name-length 1)))
              message]
	     [(and (char=? (string-ref message x) #\space)
                   (let ([subm (substring message 0 x)])
                     (or (string=? subm bot-name)
                         (string=? (metaphone subm) bot-metaphone))))
              (substring message (+ x 1) (string-length message))]
             [(memv (string-ref message x) '(#\: #\,))
              (substring message (+ x 1) (string-length message))]
             [else (loop (+ x 1))])))))

(define (strict-tokenize message)
  (map (lambda (ts)
         (string->symbol
          (list->string 
           (clean ts really-clean))))
       (map (lambda (tok)
              (string->list
               (symbol->string tok)))
            (tokenize message))))

(define (crib-match? crib message)
  (let ([strict-tokens (strict-tokenize message)]
        [strict-crib (strict-tokenize crib)])
    (call/cc
     (lambda (escape)
       (let loop ([mt strict-tokens]  [ct strict-crib] [bt escape])
         (cond [(null? ct) (escape #t)]
               [(null? mt) (bt #f)]
               [(eq? (car mt) (car ct))
                (call/cc
                 (lambda (bt)
                   (loop (cdr mt) (cdr ct) bt)))
                (loop (cdr mt) strict-crib bt)]
               [else (loop (cdr mt) strict-crib escape)]))))))

(define (full-parse bot-name message)
  (import srfi-1)
  (let* ([strict-tokens (strict-tokenize (message-text message))]
         [to-bot (or (message-is-private? message)
                     ;; Name must be first or last in a sentence
                     (string=? bot-metaphone
                           (metaphone (symbol->string (car strict-tokens))))
                     (string=? bot-metaphone 
                           (metaphone (symbol->string (last strict-tokens)))))]
         [cleaned-message (bot-clean bot-name (message-text message))])
    (values to-bot cleaned-message strict-tokens)))
          
