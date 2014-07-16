(define (datomic/query qry . sources)
  (->scm-object
   (let ((sources (jlist->jarray (->jobject sources)))
         (qry (if (string? qry) (->jstring qry) qry)))
     (j "datomic.Peer.q(qry, sources);" `((sources ,sources)
                                          (qry ,qry))))))

(define (datomic/smart-query qry . sources)
  (let ((result (apply datomic/query (flatten (list qry sources)))))
    (match result
      [((tresult)) tresult]
      [() #f]
      [anything-else result])))

(define* (datomic/connection uri (should-create? #t) (want-to-know-if-created? #f))
  (let ((did-I-create-it? (if should-create?
                              (->boolean (j "datomic.Peer.createDatabase(uri);" `((uri ,(->jstring uri)))))
                              #f)))
    (let ((connection-result (j "datomic.Peer.connect(uri);" `((uri ,(->jstring uri))))))
      (if want-to-know-if-created?
          (list did-I-create-it? connection-result)
          connection-result))))

(define (datomic/temp-id)
  (define-java-classes (<datomic.peer> |datomic.Peer|))    
  (define-generic-java-method tempid)

  (let ((<datomic.peer>-java-null (java-null <datomic.peer>)))
    ;; spec: (j "temp_id = datomic.Peer.tempid(\":db.part/user\");")
    (tempid <datomic.peer>-java-null partition)))

(define (datomic/transact conn tx)
  (define-generic-java-method transact)
  ;;spec: (j "conn.transact(tx);" `((conn ,conn) (tx ,tx)))
  (transact conn tx))

(define (datomic/transact-async conn tx)
  (define-generic-java-method transact-async |transactAsync|)
  ;;spec:
  ;;(j "conn.transactAsync(tx);" `((conn ,conn) (tx ,tx)))
  ;;(transact conn tx)
  (transact-async conn tx))

(define (datomic/db cn)
  (j "connection.db();" `((connection ,cn))))

(define (datomic/make-latest-db-retriever connection-retriever)
  (lambda ()
    (datomic/db (connection-retriever))))

(define (datomic/make-with-one-connection-included-query-function connection-retriever)
  (let ((db-retriever (datomic/make-latest-db-retriever connection-retriever)))
    (cut datomic/smart-query
         <> ; Query.
         (db-retriever) ; Recent db fetched.
         <...> ; Whatever other insanity and/or fixed parameters one may pass.
         )))

(define (datomic/smart-transact conn tx param-alist)
  (let ((final-param-alist (append param-alist `((conn ,conn)))))
    (clj (string-append "(use '[datomic.api :only [q db] :as d])
                         @(d/transact conn " tx ")")
         final-param-alist)))

(define (datomic/make-with-one-connection-included-transact-function connection-retriever)
  (cut datomic/smart-transact
       (connection-retriever) ; Current connection
       <> ; tx
       <> ; param-alist
       ))

;; Usage example - test/q does not require a connection or anything besides what
;; is required for the immediate task at hand.
(define (test-query-parametrized connection)
  (let ((test/q (datomic/make-with-one-connection-included-query-function (connection))))
    (test/q "[:find ?eid :in $data ?targetemail  :where [$data ?eid :user-email ?targetemail]]"
            "nietzsche@CaballerosDeLaTristeFigura.net")))    

; shortcuts
(define d/q datomic/query)
(define d/sq datomic/smart-query)
(define d/id datomic/temp-id)
(define d/t datomic/transact)
(define d/db datomic/db)
(define d/st datomic/smart-transact)
