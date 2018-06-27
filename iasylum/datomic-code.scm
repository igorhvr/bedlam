;;
;; Automatically convert input (sources) to jobject
;; and output to scm-object.
;;
(define (datomic/query qry-input . sources)
  (log-time ("Query execution" log-trace log-warn 300 ms)
    (->scm-object
     (let ((sources (jlist->jarray (->jobject sources)))
           (qry (if (string? qry-input)
                    (->jstring qry-input)
                    qry-input)))
       (log-trace "Will execute query: " (cut-log (->string qry))
                  "with sources:" (cut-log (jarray->string sources)))
       (let ((result (j "datomic.Peer.q(qry, sources);" `((sources ,sources)
                                                          (qry ,qry)))))
         (log-trace "=> Query result: " (cut-log (iasylum-write-string result)))
         result)))))

;;
;; DEPRECATED: This is deprecated because it's TOO DANGEROUS when the caller is expecting
;; many results as response. When the list has only one single element or nothing
;; this function simply change its behavior. The only element is returned instead the list,
;; and no result is converted to #f.
;;
;; If the caller is expecting more than one element, it forces the (unaware) caller to
;; treat different and check if the result has one element. A simple example:
;;
;; (map (lambda (e) (display e)) (datomic/smart-query ...)) would fail miserably
;; (and probably silently and late) when the result has only one or no element.
;;
(define (datomic/smart-query qry . sources)
  (log-warn "datomic/smart-query **IS DEPRECATED!** Please use datomic/smart-query-single or datomic/smart-query-multiple instead!")
  (let ((result (apply datomic/query (flatten (list qry sources)))))
    (match result
      [((tresult)) tresult]
      [() #f]
      [anything-else result])))

;;
;; Expect only one result and return this only single element or #f if it is not present.
;; This is useful if you are searching by a specific ID for example.
;;
;; If the query is searching for only one attribute so return the value of this attribute,
;; otherwise a list with the specified attributes in the query.
;;
;; Example: (datomic/smart-query-single "[:find ?v :where ?e :att ?v]" db) will return ?v and
;;          (datomic/smart-query-single "[:find ?e ?v :where ?e :att ?v]" db) will return (list ?e ?v)
;;
(define (datomic/smart-query-single qry . sources)
  (let ((result (apply datomic/query (flatten (list qry sources)))))
    (match result
      [((single-result-only-att)) single-result-only-att]
      [(tuple) tuple]
      [() #f]
      [else (throw (make-error 'datomic/smart-query "Multiple results are not expected here.
Please use datomic/smart-query-multiple instead if multiple results are expected. This is probably a collateral bug: do you have duplicated ids?"))])))

;;
;; Always will return a list of results. Use this when the result can be a set of elements.
;;
(define (datomic/smart-query-multiple qry . sources)
  (apply datomic/query (flatten (list qry sources))))

(define* (datomic/connection uri (should-create? #f) (want-to-know-if-created? #f))
  (let ((did-I-create-it? (if should-create?
                              (->boolean (j "datomic.Peer.createDatabase(uri);" `((uri ,(->jstring uri)))))
                              #f)))
    (let ((connection-result (j "datomic.Peer.connect(uri);" `((uri ,(->jstring uri))))))
      (if want-to-know-if-created?
          (list did-I-create-it? connection-result)
          connection-result))))

(define* (datomic/temp-id (partition-symbol 'db.part/user) (id #f))
  (define-java-classes (<datomic.peer> |datomic.Peer|))
  (define-generic-java-method tempid)
  
  (let ((<datomic.peer>-java-null (java-null <datomic.peer>)))
    ;; spec: (j "temp_id = datomic.Peer.tempid(\":db.part/user\");")
    (if (not id)
        (tempid <datomic.peer>-java-null (->jstring (clj-keyword->string (symbol->clj-keyword partition-symbol))))
        (tempid <datomic.peer>-java-null (->jstring (clj-keyword->string (symbol->clj-keyword partition-symbol)))
                (->jlong id)))))

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

(define (datomic/db-history db)
  (j "db.history();" `((db ,db))))

(define (datomic/as-of db t)
  (j "db.asOf(t);" `((db ,db)
                     (t ,(->jobject t)))))

(define (datomic/since db t)
  (j "db.since(t);" `((db ,db)
                      (t ,(->jobject t)))))

(define (datomic/make-latest-db-retriever connection-retriever)
  (lambda ()
    (datomic/db (connection-retriever))))

;;
;; This is deprecated: see datomic/smart-query to more information.
;; Use datomic/make-query-function-with-one-connection-included instead.
;;
(define (datomic/make-with-one-connection-included-query-function connection-retriever)
  (log-warn "datomic/make-with-one-connection-included-query-function **IS DEPRECATED!** Please read the doc for more info. Use datomic/make-query-function-with-one-connection-included instead.")
  (datomic/make-query-function-with-one-connection-included datomic/smart-query connection-retriever))

;;
;; - smart-query-lambda can be datomic/smart-query-multiple or datomic/smart-query-single depending on
;;                      what kind of result are expected.
;;
;; - since: a "t" EXCLUSIVE to exclude all tx before this date.
;; - until: a "t" INCLUSIVE to exclude all tx after this date.
;;
;; t can be a date scheme or a java.util.Date java object.
;;
;; DISCLAIMER: using "since" is not so easy as it sounds: the problem is that the lockup
;;             info is created at the moment of the tx, so if you exclude the beginning you lost this
;;             info and probably your query will return nil. You HAVE to lockup only by ID, so the solution
;;             is find the ID in the most recent db and then use since to get the entity info.
;;             See http://docs.datomic.com/filters.html for more info.
;;             If you use "since" param, please notice that this fn send the most recent db
;;             as first source and the since as second one. Your query have to look something like this:
;;
;;             [:find ?count 
;;              :in $ $since ?id 
;;              :where [$ ?e :item/id ?id]
;;                     [$since ?e :item/count ?count]]
;;
;; if you use the option "include-history" so use $history to refer the history database like:
;;
;;             [:find ?count
;;              :in $ $history ?id
;;              :where [$ ?e :item/id ?id]
;;                     [$history ?e :item/count ?count]]
;;
;; or (if since is also used):
;;
;;             [:find ?count
;;              :in $ $since $history ?id
;;              :where [$ ?e :item/id ?id]
;;                     [$history ?e :item/id ?id]
;;                     [$since ?e :item/count ?count]]
;;
(define* (datomic/make-query-function-with-one-connection-included smart-query-lambda
                                                                   connection-retriever
                                                                   (since: since #f)
                                                                   (until: until #f)
                                                                   (include-history: include-history #f))
  (let* ((current-db-retriever (datomic/make-latest-db-retriever connection-retriever))
         (last-db-retriever (if (not until)
                                current-db-retriever
                                (lambda ()
                                  (datomic/as-of (current-db-retriever)
                                                 until)))))
    (if (not include-history)
        (if (not since)
            (cut smart-query-lambda
                 <> ; Query.
                 (last-db-retriever)
                 <...> ; Whatever other insanity and/or fixed parameters one may pass.
                 )
            (cut smart-query-lambda
                 <> ; Query.
                 (last-db-retriever)
                 (datomic/since (last-db-retriever)
                                since)
                 <...> ; other params
                 ))
        (if (not since)
            (cut smart-query-lambda
                 <> ; Query.
                 (last-db-retriever)
                 (datomic/db-history (last-db-retriever))
                 <...> ; Whatever other insanity and/or fixed parameters one may pass.
                 )
            (cut smart-query-lambda
                 <> ; Query.
                 (last-db-retriever)
                 (datomic/since (last-db-retriever)
                                since)
                 (datomic/db-history (last-db-retriever))
                 <...> ; other params
                 )))))

(define (cut-log message)
  (let ((length (string-length message)))
    (if (> length 1024)
        (format "~a [...] (+total ~a chars)"
                (substring message 0 1024)
                length)
        message)))

;;
;; Differently of smart-query, this DOES NOT convert input (params) to jobject
;; and output to scm-object. You have to send java objects in params.
;;
(define* (datomic/smart-transact conn tx
                                 (param-alist '())
                                 (allow-cut-log: allow-cut-log #t))
  (log-time ("Transaction execution" log-trace log-warn 500 ms)
      (let ((final-param-alist (append param-alist `((conn ,conn)
                                                     (tx ,(if (string? tx) (clj tx param-alist) tx))))))
        (log-trace "Will execute transact" (if (and allow-cut-log
                                                    (string? tx))
                                               (cut-log tx)
                                               tx)
                   "with params:" (if allow-cut-log
                                      (cut-log (iasylum-write-string param-alist))
                                      (iasylum-write-string param-alist)))
        (let ((result (clj "(use '[datomic.api :only [q db] :as d])
                            @(d/transact conn tx)"
                           final-param-alist)))
          (log-trace "=> Transaction result " (if allow-cut-log
                                                  (cut-log (iasylum-write-string result))
                                                  (iasylum-write-string result)))
          result)))
  )

(define (datomic/make-transact-function-with-one-connection-included connection-retriever)
  (cut datomic/smart-transact
       (connection-retriever) ; Current connection
       <>    ; tx
       <...> ; param-alist (or nothing)
       ))

(define (datomic/uuid)
  (j "datomic.Peer.squuid()"))

(define (datomic/extract-time-millis-from-uuid datomic-uuid)
  (j "datomic.Peer.squuidTimeMillis(squuid)"
     `((squuid ,datomic-uuid))))

(define (datomic/tx->t tx)
  (->scm-object (j "datomic.Peer.toT(tx);" `((tx ,(->jlong tx))))))

;;
;; Usage example - test/q does not require a connection or anything besides what
;; is required for the immediate task at hand.
;;
(define (test-query-parametrized connection)
  (let ((test/q (datomic/make-with-one-connection-included-query-function (connection))))
    (test/q "[:find ?eid :in $data ?targetemail  :where [$data ?eid :user-email ?targetemail]]"
            "nietzsche@CaballerosDeLaTristeFigura.net")))    

;; partition-symbol CANNOT include the colon ":" in the symbol/keyword
;; example:
;; (datomic/id 'example)
;;
;; id -1 to -1000000, inclusive, are reserved for
;; user-created tempids. Use other negative number or just don't
;; pass the ID to create a new secure fresh one.
;;
;; partition-symbol is option only to backward compatibility, it SHOULD be defined
;; otherwise an ERROR is logged.
;;
;; note: #db/id[<partition> [<id>]] macro also creates a tempid, it was used before (see history).
;;
(define* (datomic/temp-id-slow (partition-symbol 'db.part/user) (id #f))
  (if (equal? partition-symbol 'db.part/user)
      (log-error "*** Please use it ONLY FOR TESTS! Define a partition explicitly. db.part/user is only for TESTS purposes."))
  (let ((params `((partition ,(symbol->clj-keyword partition-symbol)))))
    (if (not id)
        (clj "(d/tempid partition)" params)
        (clj "(d/tempid partition id)" (append params `((id ,(->jlong id))))))))

(define-nongenerative-struct transaction-set a9c14080-0fb1-11e4-99e0-78ca39b1ca29
  (transaction-string parameters))

;;
;; It is a composition of a tx in string format (starts with empty string)
;; and a list of parameters (starts with empty list).
;;
;; See datomic/push-transaction! and datomic/extract-transaction-and-parameters-pair
;;
(define (datomic/make-empty-transaction-set)
  (make-transaction-set "" '()))

;;
;; Adds a transaction (in string format) and a list of parameters into a transaction-set.
;; The transaction-string would be a clojure map or list like "{:key value :key2 value2}"
;; or "[:fn p1 p2 ...]".
;;
;; parameters should be something like `((key ,value) (key2 ,value2)) - it is the same
;; input as datomic/smart-transact except by in datomic/smart-transact the tx should be
;; a list of maps or lists.
;;
;; See also datomic/extract-transaction-and-parameters-pair
;; and datomic/make-empty-transaction-set
;;
(define (datomic/push-transaction! transaction-set
                                   transaction-string
                                   parameters)
  (set-transaction-set-transaction-string! transaction-set
                                           (string-append (transaction-set-transaction-string transaction-set)
                                                          transaction-string))
  (set-transaction-set-parameters! transaction-set
                                   (append (transaction-set-parameters transaction-set)
                                           parameters))
  transaction-set)

;;
;; Transform the final string into a list (just add [ ] around) and return
;; the pair formed by tx string + params, so (car (datomic/extract-transaction-and-parameters-pair ...))
;; is the first parameter of datomic/smart-transact and (cdr (datomic/extract-transaction-and-parameters-pair ...))
;; is the second one.
;;
;; See datomic/push-transaction!
;;
(define (datomic/extract-transaction-and-parameters-pair transaction-set)
  `(,(string-append "[" (transaction-set-transaction-string transaction-set) "]")
    .
    ,(transaction-set-parameters transaction-set)))

;;
;; Get a value from a lazy entity.
;;
;; - lazy-entity - To get a lazy entity see datomic/get-entity
;; - field-name - It is a string like ":common/has-uuid"
;;
(define (datomic/get-value lazy-entity field-name)
  (define-generic-java-method |get|)
  (->scm-object
   ; spec (j "lazyen.get(field);" `((lazyen ,lazy-entity) (field ,(->jstring field-name))))
   (get lazy-entity (->jstring field-name))))

;;
;; Return a datomic.query.EntityMap object.
;; See http://docs.datomic.com/entities.html to see how to use that.
;; See also: http://docs.datomic.com/javadoc/datomic/Entity.html
;;
(define (datomic/get-entity database entity-id)
  (define-generic-java-method |entity|)
  (define-generic-java-method |valueOf|)
  (define-java-class |java.lang.Long|)

  (or (and (number? entity-id)
           (entity database (|valueOf| (java-null |java.lang.Long|) (->jlong entity-id))))
      (j "db.entity(eid);" `((db ,database) (eid ,(->jobject entity-id))))))

;;
;; Return a list of datomic tx ids of an specific entity sorted
;; by the most recent to the oldest one.
;;
(define (datomic/get-history connection-retriever entity-id)
  (sort < (flatten (datomic/smart-query-multiple
                    "[:find ?tx
                      :in $ ?e
                      :where [?e _ _ ?tx]]"
                    (datomic/db-history (datomic/db (connection-retriever))) entity-id))))

;;
;; Get past versions of an entity.
;;
;; If depth is zero get the current value. If 1, the one before, and so on.
;;
(define* (datomic/travel-machine connection-retriever
                                 entity-history
                                 entity-id
                                 depth)
  (let ((entity-history-length (length entity-history)))
    (cond [(< depth 0)
           (throw (make-error 'datomic/time-machine "depth should be greater than zero: ~a" depth))]

          [(>= depth entity-history-length)
           (throw (make-error 'datomic/time-machine "depth should be less than ~a: ~a" entity-history-length depth))]

          [else (datomic/get-entity
                 (datomic/as-of (datomic/db (connection-retriever))
                                (datomic/tx->t (list-ref entity-history depth)))
                 entity-id)])))

;;
;; Valid examples of input:
;;
;;  `((db/id . ,(datomic/temp-id 'army))
;;    (test . ,(random-string)))
;;
;;  => [{:db/id #db/id[:army]
;;       :test/test random-code}]
;;
;;  `((db/add ,(datomic/temp-id 'army) test (random-string)))
;;
;;  => [[:db/add #db/id[:army] :test/test random-code]]
;;
(define (scheme->datomic tx)
  (let continue ((tx tx))
    (cond [(null? tx) (list->persistent-vector '())]
          [(symbol? tx) (symbol->clj-keyword tx)]
          [(and (integer? tx) (exact? tx))
           (j "new java.lang.Long(a);" `((a ,(->jobject tx))))]
          [(number? tx)
           (j "new java.lang.Double(a);" `((a ,(->jobject tx))))]
          [(string? tx) (->jstring tx)]
          [(boolean? tx)
           (j "new java.lang.Boolean(a);" `((a ,(->jobject tx))))]
          [(java-object? tx) tx]
          [(pure-alist? tx) (alist->persistent-map (map (lambda (x)
                                                          (list (continue (car x))
                                                                (continue (cdr x))))
                                                        tx))]
          [(list? tx) (list->persistent-vector (map (lambda (x)
                                                      (continue x))
                                                    tx))]
          [else (->jobject tx)])))

(create-shortcuts (datomic/query -> d/q)
                  (datomic/smart-query -> d/sq) ; <-- deprecated!
                  (datomic/smart-query-single -> d/sq1)
                  (datomic/smart-query-multiple -> d/sqm)
                  (datomic/temp-id -> d/id)
                  (datomic/transact -> d/t)
                  (datomic/db -> d/db)
                  (datomic/smart-transact -> d/st))

