(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/json))
(require-extension (srfi 19)) ; date & time
(require-extension (lib sql/query))
(require-extension (lib sql/jdbc))

(module iasylum/jdbc
  (jdbc/load-drivers jdbc/map-result-set jdbc/get-connection jdbc/for-each-result-set
                     result-set->iterator
                     execute-jdbc-query execute-jdbc-update execute-jdbc-something
                     get-data get-data-with-headers-at-each-line data-with-headers-at-each-line->json
                     get-data/table get-data-result->table get-data-result->tables
                     for-each-data
                     map-each-data
                     jdbc/for-each-triple
                     create-thread-local-jdbc/get-connection-function
                     pool-datasource
                     datasource/get-connection-function jdbc/retrieve-connection-and-run
                     jdbc-connection-close
                     )

  
  (define (jdbc/load-drivers)
    (j "System.setProperty(\"log4jdbc.dump.sql.maxlinelength\", \"0\");")
    (and
     (with-failure-continuation (lambda (e p) (list e p)) (lambda () (j "Class.forName(\"org.postgresql.Driver\");")))
     (with-failure-continuation (lambda (e p) (list e p)) (lambda () (j "Class.forName(\"net.sf.log4jdbc.DriverSpy\");")))
     (with-failure-continuation (lambda (e p) (list e p)) (lambda () (j "Class.forName(\"net.sourceforge.jtds.jdbc.Driver\");")))
     (with-failure-continuation (lambda (e p) (list e p)) (lambda () (j "Class.forName(\"com.mysql.jdbc.Driver\");")))
     #t))
  
  (define (jdbc/get-connection url username password)
    (jdbc/load-drivers)

    (j "java.sql.DriverManager.getConnection(url,username,password);"
       `(
         (url ,(->jstring url))
         (username ,(->jstring username))
         (password ,(->jstring password))
         )))

  (define-generic-java-method get |get|)
  (define-generic-java-method set |set|)
  
  (define (create-thread-local-jdbc/get-connection-function url username password)
    (jdbc/load-drivers)

    (let ((tl
           (j "mtl=new ThreadLocal() {
                   protected synchronized Object initialValue() {                       
                       return java.sql.DriverManager.getConnection(url,username,password);
                   }
               }; mtl;"
       `(
         (url ,(->jstring url))
         (username ,(->jstring username))
         (password ,(->jstring password))
         ))))
      (lambda ()
        (let ((result (get tl)))
          (when (java-null? result) (set tl (jdbc/get-connection url username password)))
          (let ((to-return (get tl)))            
            to-return)))))

  ;; Sample usage: (pool-datasource "jdbc:postgresql://somehostsomewhere:5432/databasename" "username" "password")
  ;; Use http://docs.oracle.com/javase/7/docs/api/javax/sql/DataSource.html#getConnection%28%29 later to retrieve a connection
  ;; from this pool. And close it to return it to the pool.
  (define pool-datasource
    (lambda* (jdbc-url user password (maximumPoolSize: maximumPoolSize 10))
             (j "config = new com.zaxxer.hikari.HikariConfig();
                 config.setJdbcUrl(jdbcurl);
                 config.setUsername(tuser);
                 config.setPassword(tpassword);
                 config.setMaximumPoolSize(mps);
                 config.addDataSourceProperty(\"cachePrepStmts\", \"true\");
                 config.addDataSourceProperty(\"prepStmtCacheSize\", \"250\");
                 config.addDataSourceProperty(\"prepStmtCacheSqlLimit\", \"2048\");
                 config.addDataSourceProperty(\"useServerPrepStmts\", \"true\");
         
                 new com.zaxxer.hikari.HikariDataSource(config);"
                `((jdbcurl ,(->jstring jdbc-url)) (tuser ,(->jstring user)) (tpassword ,(->jstring password))
                  (mps ,(->jobject maximumPoolSize))))))

  (define (datasource/get-connection-function ds)
    (lambda ()
      (j "ds.getConnection();" `((ds ,ds)))))

  (define (jdbc/retrieve-connection-and-run connection-retriever function)
    (assert connection-retriever "A non-null connection-retriever must be provided. It will be used to fetch a connection, execute the provided function, a process after which close will be called on the connection.")
    (let ((conn (connection-retriever)))
      (dynamic-wind
          (lambda () #t)
          (lambda () (function conn))
          (lambda () (jdbc-connection-close conn)))))
  
  (define-generic-java-method close)

  (define (jdbc-connection-close conn)
    (close conn))

  (define-generic-java-method gmd |getMetaData|)
  
  (define-generic-java-method get-column-count |getColumnCount|)
  
  (define column-count (lambda (result-set) (->number (get-column-count (gmd
                                                                    result-set)))))
  
  (define-generic-java-method get-column-name |getColumnName|)
  
  (define column-name (lambda (i result-set) (->string (get-column-name (gmd
                                                                    result-set) (->jint i)))))
  
  (define-generic-java-method get-column-type-name |getColumnTypeName|)
  
  (define column-type (lambda (i result-set) (->string (get-column-type-name
                                                   (gmd result-set) (->jint i)))))

  (define-generic-java-method next)

  (define-generic-java-method get-object)
  
  (define (jdbc/for-each-result-set function result-set)    
    (define (more-data?)
      (->boolean (next result-set)))
    (let l ()
      ;;(debug more-data? result-set)
      (when (more-data?)
        ;;(debug function result-set)
        (function 
         (reverse
          (let l ((i 1) (m (column-count result-set))(acc '()))
            (if (not (> i m))                
                (l (+ i 1) m (cons
                              (cons
                               (column-name i result-set)
                               (->scm-object (get-object result-set (->jint i)))
                               )
                              acc))
                acc)))
         )
         (l))))
        
  (define (jdbc/map-result-set function result-set)
    (let ((result (list)))
      (jdbc/for-each-result-set
       (lambda (v) (set! result (cons (function v) result)))
       result-set)
      (reverse result)
      ))
  
  (define-generic-java-method create-statement)
  (define new-statement
    (lambda (connection)
      (create-statement connection)))

  (define-generic-java-method prepare-statement)
  (define-java-class <java.sql.result-set>)  
  (define-generic-java-field-accessor :TYPE_FORWARD_ONLY |TYPE_FORWARD_ONLY|)
  (define-generic-java-field-accessor :CONCUR_READ_ONLY |CONCUR_READ_ONLY|)
  (define new-prepared-statement
    (lambda (connection sql)
      (prepare-statement connection sql (:TYPE_FORWARD_ONLY (java-null <java.sql.result-set>)) (:CONCUR_READ_ONLY (java-null <java.sql.result-set>)))))
  


  (define-generic-java-method get-column-type)
  (define-generic-java-method get-meta-data)

  (define (result-set->iterator rs)
    (j "
     import java.sql.ResultSet;
     import java.sql.SQLException;
     import java.util.ArrayList;
     import java.util.Iterator;
     import java.util.List;
     
     public class ResultSetIterator implements Iterator {
         private ResultSet resultSet;
         private boolean hasNext;
     
         public ResultSetIterator(ResultSet resultSet) {
             this.resultSet=resultSet;
             try {
                 this.hasNext=resultSet.next();
             } catch (SQLException e) {
                 throw new RuntimeException(e);
             }
         }
     
         public boolean hasNext() {
             return hasNext;
         }
     
         public Object next() {
             try {
                 int columns=resultSet.getMetaData().getColumnCount();
                 List result=new ArrayList(columns);
     
                 for(int i=1;i<=columns;i++) {                    
                     result.add(resultSet.getObject(i));
                 }

                 hasNext=resultSet.next();
     
                 return result;
             }catch(Exception e){
                 throw new RuntimeException(e);
             }
         }
     
         public void remove() {
             throw new RuntimeException();
         }
     }
     ")
  (j "new ResultSetIterator(rs);" `((rs ,rs))))


  (define-generic-java-method execute-query)
  (define-generic-java-method execute-update)
  (define-generic-java-method set-fetch-size)

  (define execute-jdbc-query
    (lambda* (connection query (vars #f) (fetch-size #f))
             (execute-jdbc-something execute-query connection query vars fetch-size)))

  ;; execute-jdbc-update is a draft. You should still probably use sql/execute-update instead.
  (define execute-jdbc-update
    (lambda* (connection query (vars #f) (fetch-size #f))
             (execute-jdbc-something execute-update connection query vars fetch-size)))

  (define-java-classes
    (<sql-date> |java.sql.Date|)
    (<jobject> |java.lang.Object|)
    (<sql-timestamp> |java.sql.Timestamp|))
  
  (define (jdbc/->jobject value)
    (cond 
     ((date? value)
      (let ((t (date->time-utc value)))
        ;(java-new <sql-date> (->jlong (* 1000 (time-second t))))
        (java-new <sql-timestamp> (->jlong (* 1000 (time-second t))))
        ))
     ((time? value)
      (java-new <sql-timestamp> (->jlong (* 1000 (time-second value)))))
     ((null? value)
      (java-null <jobject>))
     (else (->jobject value))))
  
  (define execute-jdbc-something
    (lambda* (something connection query (vars #f) (fetch-size #f))
             (let ((stmt (if vars
                             (new-prepared-statement connection (->jstring query))
                             (new-statement connection))))
               (when fetch-size (set-fetch-size stmt (->jint fetch-size)))
               
               (if vars
                   (begin
                     (for-each
                      (lambda (v)
                        (match-let ( ( (tindex tvalue) v ) )
                                   (begin
                                     (let ((objectindex (->jobject tindex))
                                           (objectvalue (jdbc/->jobject tvalue)))
                                       (j "stmt.setObject(objectindex, objectvalue);"
                                          `((stmt ,stmt)
                                            (objectindex ,objectindex)
                                            (objectvalue ,objectvalue)))))))
                      vars)
                     (something stmt))
                   (something stmt (->jstring query))))))

  (define get-data
    (lambda* (connection query (vars #f))
             (define (read-metadata p)
               (let l ((i 1) (m (->number (get-column-count p))))
                 (if (> i m) '()
                     (cons
                      (cons (->scm-object (get-column-name p (->jint i)))
                            (->scm-object (get-column-type-name p (->jint i))))
                      (l (+ i 1) m)))))
             (let ((rs (execute-jdbc-query connection query vars) ))
               (let ((rs-md (read-metadata (get-meta-data rs)))
                     (data (iterable->list (result-set->iterator rs) (lambda (v) (->scm-object v)))))
                 (cons rs-md data)))))

  (define get-data/table
    (lambda* (connection query (vars #f))
             (let* ((data (get-data connection query vars)))
               (get-data-result->table data))))

  (define get-data-result->table/escape-newlines (lambda (str) (irregex-replace/all (irregex 'newline) str "\\n")))

  ;; This generates a string with a table built in a format similar
  ;; to the example below. Useful for debugging, mostly.

  ;;  (d/n (get-data-result->table '((("key" . "varchar") ("value" . "varchar")) ("SOME RANDOM \nEXAMPLE" "YES") ("SECOND LINE" "NO"))))
  ;;
  ;; | ---                   | ---       |
  ;; | key                   | value     |
  ;; | (varchar)             | (varchar) |
  ;; | ---                   | ---       |
  ;; | SOME RANDOM \nEXAMPLE | YES       |
  ;; | SECOND LINE           | NO        |
  ;; | ---                   | ---       |
  (define* (get-data-result->table
            (escape-newlines: escape-newlines get-data-result->table/escape-newlines)
            get-data-result)
    (and-let* ((metadata-row (car get-data-result))
               (column-indexes (list-ec (: i 0 (length metadata-row)) i))
               (data-rows (cdr get-data-result))
               
               (data-columns
                (match metadata-row
                     [((field-name . field-type) ...)
                      (pam (zip field-name field-type column-indexes)
                           (match-lambda [(field-name field-type column-number)
                                     (string-append* "---\n"
                                                     (escape-newlines field-name) "\n"
                                                     (if field-type (string-append "(" (escape-newlines field-type) ")" "\n") "")
                                                     "---\n"
                                                     (apply string-append
                                                            (add-between-list "\n"
                                                                              (map escape-newlines
                                                                                   (map (cute list-ref <> column-number) data-rows))))
                                                     "\n---"))])]
                     [else (throw (make-error "unexpected metadata format"))]))
               ;; 1 or 2 lines for title + 3 horizontal spacer + lines for content.
               (separator-column
                (let ((has-field-type (any identity (match metadata-row [((field-name . field-type) ...) field-type]))))
                  (apply string-append
                         (add-between-list "\n" (list-ec (: i 0 (+ 1 (if has-field-type 1 0) 3 (length data-rows))) " | "))))))
  (fmt #f (apply tabular (append (list (dsp separator-column))
                                 (map dsp (add-between-list separator-column data-columns))
                                 (list (dsp separator-column)))))))

; This version splits in multiple tables, each with a header, at a set maximum size.
;
; Usage example: (d/n (get-data-result->tables 'rows-per-table: 1
;                              '((("key" . "varchar") ("value" . "varchar"))
;                                ("SOME RANDOM \nEXAMPLE" "YES")
;                                ("SECOND LINE" "NO"))))
(define* (get-data-result->tables
          (escape-newlines: escape-newlines get-data-result->table/escape-newlines)
          (rows-per-table: rows-per-table)
          get-data-result)
  (and-let* ((metadata-row (car get-data-result))
             (list-of-smaller-headerless-results (split-list-in-groups 'group-size: rows-per-table (cdr get-data-result)))
             (list-of-smaller-get-data-results (map (lambda (p) (cons metadata-row p)) list-of-smaller-headerless-results)))
    (map (cute get-data-result->table 'escape-newlines: escape-newlines <>) list-of-smaller-get-data-results)))

  (define get-data-with-headers-at-each-line
    (lambda* (connection query (vars #f))
             (let* ((data (get-data connection query vars))
                    (headers (car data))
                    (the-data (cdr data)))
               (pam the-data
                    (lambda (v)
                      (zip headers v))))))

  (define (data-with-headers-at-each-line->json sd)
    (scheme->json
     `#((result . ,(pam sd (lambda (v)
                             (list->vector
                              (pam v
                                   (match-lambda
                                    (((fieldname . fieldtype) fieldvalue)
                                     (cons fieldname
                                           (cond ((date? fieldvalue)
                                                  (date->string fieldvalue "~4"))
                                                 ((null? fieldvalue)
                                                  (void))                                                 
                                                 (else fieldvalue)))))))))))))
      

  (define (for-each-data connection query vars proc)
    (define (read-metadata p)
      (let l ((i 1) (m (->number (get-column-count p))))
        (if (> i m) '()
            (cons
             (cons (->scm-object (get-column-name p (->jint i)))
                   (->scm-object (get-column-type-name p (->jint i))))
             (l (+ i 1) m)))))
    (let ((rs (execute-jdbc-query connection query vars) ))
      (let ((rs-md (read-metadata (get-meta-data rs))))
            (for-each-iterable (result-set->iterator rs) (lambda (v) (proc (list rs-md (->scm-object v))))))))

  (define (map-each-data connection query vars proc)
    (define result (list))
    (define (proc v)
      (set! result (cons v result)))
    (for-each-data connection query vars proc)
    result)

  ;;; Triples with column name, column type and unwrapped column value.
  (define (jdbc/for-each-triple function result-set)
    (define (more-data?)
      (->boolean (next result-set)))
    (define metadata (get-meta-data result-set))
    (let l ()
      ;;(debug more-data? result-set)
      (when (more-data?)
        ;;(debug function result-set)
        (function 
         (reverse
          (let l ((i 1) (m (column-count result-set))(acc '()))
            (if (not (> i m))                
                (l (+ i 1) m (cons
                              (cons
                               (column-name i result-set)
                               (cons
                                (->scm-object (get-column-type-name metadata (->jint i)))
                                (cons
                                 (get-object result-set         (->jint i))
                                 '()))
                               )
                              acc))
                acc)))
         )
         (l))))

  (define (jdbc/map-triple function result-set)
    (let ((result (list)))
      (jdbc/for-each-triple
       (lambda (v) (set! result (cons (function v) result)))
       result-set)
      (reverse result)
      ))
    
  
  )
