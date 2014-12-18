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
                     for-each-data
                     map-each-data
                     jdbc/for-each-triple
                     create-thread-local-jdbc/get-connection-function
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
                                     (j "stmt.setObject(objectindex, objectvalue);"
                                        `((stmt ,stmt)
                                          (objectindex ,(->jobject tindex))
                                          (objectvalue ,(->jobject tvalue)))))))
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

  (define get-data-with-headers-at-each-line
    (lambda* (connection query (vars #f))
             (let* ((data (get-data connection query))
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
                                           (if (date? fieldvalue)
                                               (date->string fieldvalue "~4")
                                               fieldvalue)
                                           )))))))))))
      

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
