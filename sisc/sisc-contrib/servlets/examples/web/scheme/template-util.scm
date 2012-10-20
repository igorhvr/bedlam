(load "util.scm")

(import string-io)

(define-java-classes
  <java.util.hashtable>
  <java.util.vector>)

(define-generic-java-methods
  put
  add)

;;convert scheme data structure into template data structure
(define (->template data)
  (cond
    [(java-object? data)                ;pass through
     data]
    [(list? data)                       ;convert to hashtable
     (let ([ht (java-new <java.util.hashtable>)])
       (for-each (lambda (e)
                   (put ht (->template (car e)) (->template (cdr e))))
                 data)
       ht)]
    [(vector? data)                     ;convert to vector
     (let ([v (java-new <java.util.vector>)])
       (for-each (lambda (e) (add v (->template e))) (vector->list data))
       v)]
    [else                               ;convert to string
      (->jstring (call-with-output-string (lambda (p)
                                            (display data p))))]))

;;

(define-generic-java-methods
  get-request-dispatcher
  forward)

(define (fill-template template data request response)
  (set-attribute request (->jstring "templateData") (->template data))
  (forward (get-request-dispatcher
            request
            (->jstring (string-append "/templates/" template ".jsp")))
           request
           response))

(define (display-form form data)
  (call/cc (lambda (k) (display-page form
                                     (cons `(cont . ,(store-k k))
                                           data)))))

(define (display-page page data)
  (fill-template page data (current-request) (current-response))
  ((current-return) #f))
