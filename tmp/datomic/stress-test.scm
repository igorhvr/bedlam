(define datomic-uri "datomic:dev://localhost:4334/stress-test")

(define iasylum-bedlam-location (string-append (current-directory) "/idm/bedlam/"))
(load (string-append iasylum-bedlam-location "iasylum/init.scm"))

;;(j "System.setProperty(\"datomic.memcachedServers\", \"z-memcached.fwe8c3.0001.use1.cache.amazonaws.com:11211,m-h.fwe8c3.0001.use1.cache.amazonaws.com:11211\");")
(j "System.setProperty(\"datomic.txTimeoutMsec\", \"100000\");")
(j "System.setProperty(\"datomic.objectCacheBytes\", \"1073741824\");") ;; 1GB

(define (string-tx->datomic datomic-connection str)
  (let ((tx (clj "(def v (read-string mstr)) v" `((mstr ,(->jstring str))))))    
    (d/transact datomic-connection tx)))

(define partition (->jstring ":db.part/user"))

(define-java-classes (<datomic.peer> |datomic.Peer|))
(define <datomic.peer>-java-null (java-null <datomic.peer>))

(define-generic-java-method tempid)
(define (temp-id)
  (tempid <datomic.peer>-java-null partition))

(define prepare-transaction-key (->jstring ":db/id"))

(define-java-classes <java.util.array-list>)
(define-generic-java-method s-put |put|)
(define-generic-java-method add)
(define (prepare-transaction data)
  (let ((map (key-value->jmap data)))
    (s-put map prepare-transaction-key (temp-id))
    (let ((d (java-new <java.util.array-list> (->jint 1))))
      (add d map) d)
    ))

(define (datomic-connection uri)
  (j "try {System.out.println(datomic.Peer.createDatabase(uri));}catch(Exception e){}
      datomic.Peer.connect(uri);"
     `((uri ,(->jstring uri)))))

(define (d/q conn qry)
  (j "datomic.Peer.q(qry, conn.db());" `((conn ,conn) (qry ,(->jstring qry)))))

(define (d/q-single conn qry)
  (j "datomic.Peer.q(qry, conn.db()).iterator().next().get(0);" `((conn ,conn) (qry ,(->jstring qry)))))

(define-generic-java-method transact)
(define (d/transact conn tx) (transact conn tx))

(define-generic-java-method transact-async |transactAsync|)
(define (d/transact-async conn tx)
  (transact-async conn tx))
          
(define datomic-schema "[ { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/surrogate_key :db/index  true :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/subscription_id :db/valueType :db.type/uuid :db/index true :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/configuration_id :db/index true :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/phone :db/index true :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/enabled :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/origin_id :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/subscription_status_id :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/current_lifecycle_id :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/creation_date :db/index true :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/extra_info :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/created_as_free_user :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/ad_partner :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/actived_date :db/index true :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/billing_count :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/timeout_date :db/index true :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/cancelled_date :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/total_credit :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/renew_count :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/billing_share :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/currency_id :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/ad_detail :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/last_successfully_transaction :db/index true :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/reference_id :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/external_reference_id :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/cpa :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/related_id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/charge_id :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/last_renew_attempt :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/change_date :db/index true :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/protocol :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/inserted_time :db/valueType :db.type/instant :db/index true :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/balance :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/last_balance_changed :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/last_publication_processed :db/valueType :db.type/instant :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/debt_consumed_today :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/total_debt_consumed :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/sequential_index :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/user_agent :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/cancel_after_promotion :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/DC :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } { :db/id #db/id[:db.part/db] :db/ident :sbs.subscription/optin :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db } ]")


(define (get-max-datomic-change_date) (->scm-object (d/q-single dc "[:find (max ?v) :where [_ :sbs.subscription/change_date ?v]]")))

(define (get-max-datomic-surrogate_key) (->scm-object (d/q-single dc "[:find (max ?v) :where [_ :sbs.subscription/surrogate_key ?v]]")))


(define-generic-java-method size)
(define (datomic-stress-test)
  (define bi (open-binary-input-file "subscriber_data_small_sample.sisc.serialized"))
  (define dc (datomic-connection datomic-uri))

  (with/fc (lambda params (d "\nIgnoring errors " d)) (lambda () (string-tx->datomic dc datomic-schema)))
  
  (let* ((dataf
          (let ((result (list))
                (read-element (lambda () (with/fc (lambda v #f) (lambda () (call-with-serial-input-port bi (lambda (p) (deserialize p))))))))
            (let loop ((data (read-element)))
              (when data
                (set! result (cons data result)) (loop (read-element))))
            result))
         (work-queue (make-queue 16384))
         (producer-thread-handle
          (thread/spawn
           (lambda ()
             (with/fc
              (lambda p (error "Crashing. Reason:" p))
              (lambda ()
                (let repeat ()
                  (for-each (lambda (v) (work-queue 'put v)) dataf)
                  (repeat)))))))
         (concurrent-counter (concurrent-semaphore))
         (threads 10)
         (consumer (lambda (v)
                     (concurrent-counter 'inc)
                     (let ((c-v (concurrent-counter)))
                       (when (= 0 (modulo c-v 16384))
                         (log-info "Processing position at " c-v "... Internal queue size: " (->number (size (work-queue 'inner-queue))))))
                     (let ((data-for-insertion (prepare-transaction v)))
                       (d/transact-async dc data-for-insertion)))))
    (j "Thread.sleep(1000);")
    (for-each (lambda (discard) (start-worker consumer work-queue #t #t)) (iota threads))
    (let r ()      
      (j "Thread.sleep(1000);") (r))))

(d "\n\nWe will now start a few REPLs attached to this JVM...\n")
(d "\n" (nrepls) "\n")
(j "Thread.sleep(10000);")
(d "\n\nAbout to start datomic stress test... will print an update every 16384 rows (set of facts representing a single imported-from-SQL-table row) processed.")

(datomic-stress-test)