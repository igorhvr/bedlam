;; A simple SISC interface to the JDBC API
;;
;; Results are returned as a stream of 'result set accessor' procedures,
;; each of which can be applied to an integer (column number) or
;; a string (column label).
;;
;; Currently only supports SQL number and string types.
(module jdbc (jdbc/connect jdbc/prepare-statement
                           jdbc/execute-query jdbc/execute jdbc/close
                           set-int set-string set-double set-float)
  (import s2j)
  (import streams)
  (define-java-classes
    <java.sql.connection>
    <java.sql.prepared-statement>
    <java.sql.result-set>
    <java.sql.driver-manager>
    <java.sql.types>)
  (define-generic-java-methods (jdbc/close close)
    prepare-statement
    execute-query
    execute-update
    get-connection
    get-meta-data
    get-type
    get-string
    get-int
    get-float
    get-double
    set-string
    set-int
    set-float
    set-double
    get-column-type
    get-column-count
    get-column-label
    delete-row
    next)
  (define type-conversions #f)
  (define (access-constant class cname)
    ((generic-java-field-accessor cname) (java-null class)))
  (define (list-index ls e)
    (cond [(null? ls) #f]
          [(equal? (car ls) e) 0]
          [else (+ 1 (list-index (cdr ls) e))]))
  (define (jdbc/connect jdbc-datasource)
    (get-connection (java-null <java.sql.driver-manager>) (->jstring jdbc-datasource)))
  (define jdbc/execute execute-update)
  (define (jdbc/execute-query statement)
    (let* ((rs (execute-query statement))
           (md (get-meta-data rs))
           (cc (->number (get-column-count md)))
           (cn (map (lambda (i)
                      (->string (get-column-label md (->jint i))))
                      (cdr (iota (+ cc 1))))))
      (let ((types-conv (map (lambda (t)
                               (cdr (assoc (->number (get-column-type md (->jint (+ t 1)))) type-conversions)))
                             (iota cc))))
        (letrec ((build-row-accessor 
                  (lambda (rs)
                    (lambda (field-id)
                      (let* ((field-pos 
                              (cond [(eq? field-id 'delete) 
                                     (delete-row rs)]
                                    [(number? field-id)
                                     field-id]
                                    [(string? field-id)
                                     (+ (list-index cn field-id) 1)]
                                    [else (error 'result-set "columns are indexed by positive non-zero integers and by field-name strings.")]))
                             (conv-record (list-ref types-conv
                                                    (- field-pos 1))))
                        ((cdr conv-record) ((car conv-record) 
                                            rs (->jint field-pos)))))))
                 (fetch-next-row
                  (lambda ()
                    (if (->boolean (next rs))
                        (stream-cons (build-row-accessor rs)
                                     (fetch-next-row))
                        '()))))
          (fetch-next-row)))))
  (define (jdbc/prepare-statement connection query . updatable)
    (if (and (not (null? updatable)) (car updatable))
        (prepare-statement connection (->jstring query)
                           (<java.sql.result-set> '|TYPE_FORWARD_ONLY|)
                           (<java.sql.result-set> '|CONCUR_UPDATABLE|))
        (prepare-statement connection (->jstring query))))
                                                       
  (set! type-conversions
        `((,(->number (access-constant <java.sql.Types> '|INTEGER|)) . (,get-int . ,->number))
          (,(->number (access-constant <java.sql.Types> '|FLOAT|)) . (,get-float . ,->number))
          (,(->number (access-constant <java.sql.Types> '|DOUBLE|)) . (,get-double . ,->number))
          (,(->number (access-constant <java.sql.Types> '|DECIMAL|)) . (,get-double . ,->number))
          (,(->number (access-constant <java.sql.Types> '|NUMERIC|)) . (,get-double . ,->number))
          (,(->number (access-constant <java.sql.Types> '|REAL|)) . (,get-float . ,->number))
          (,(->number (access-constant <java.sql.Types> '|VARCHAR|)) . (,get-string . ,->string))
          (,(->number (access-constant <java.sql.Types> '|CHAR|)) . (,get-string . ,->string))))
  (java-class '|org.postgresql.Driver|))


