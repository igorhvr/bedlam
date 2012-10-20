(import s2j)
(import generic-procedures)
(import hashtable)

;;various servlet / session / request / response methods
(define-generic-java-methods
  get-attribute
  set-attribute
  get-parameter
  get-session
  get-servlet-context
  get-real-path)
(define-generic-java-field-accessors
  :get-fn
  :post-fn
  :put-fn
  :delete-fn)
(define-generic-java-field-modifiers
  :get-fn!
  :post-fn!
  :put-fn!
  :delete-fn!)

(define current-request (make-parameter #f))
(define current-response (make-parameter #f))
(define current-return (make-parameter #f))

(define (get-k-data session)
  (let ([k-data (get-attribute session (->jstring "kData"))])
    (if (java-null? k-data)
        (let ([k-data (cons 0 (make-hashtable eq?))])
          (set-attribute session
                         (->jstring "kData")
                         (java-wrap k-data))
          k-data)
        (java-unwrap k-data))))

(define (access-k-data f)
  (let ([session (get-session (current-request) (->jboolean #t))])
    (java-synchronized
     session
     (lambda () (f (get-k-data session))))))

(define (store-k k)
  (access-k-data
   (lambda (k-data)
     (let* ([counter (car k-data)]
            [res (string-append "cont" (number->string counter))])
       (hashtable/put! (cdr k-data) (string->symbol res) k)
       (set-car! k-data (+ counter 1))
       res))))
(define (fetch-k n)
  (access-k-data
   (lambda (k-data)
     (hashtable/get (cdr k-data) (string->symbol n)))))

(define (make-dispatcher default-page)
  (lambda (request response)
    (current-request request)
    (current-response response)
    (set! request #f)
    (set! response #f)
    (if (call/cc (lambda (k) (current-return k) #t))
        (let ([k (get-parameter (current-request) (->jstring "cont"))])
          (if (java-null? k)
              (default-page)
              ((fetch-k (->string k))))))))
  
(define (get-param p)
  (->string (get-parameter (current-request) (->jstring p))))

(define (servlet-load servlet file)
  (let ([cd (current-directory)])
    (dynamic-wind
     (lambda () (current-directory
                 (->string (get-real-path (get-servlet-context servlet)
                                          (->jstring "")))))
     (lambda () (load file))
     (lambda () (current-directory cd)))))
