(require-extension (lib iasylum/jcode))
(require-extension (srfi 19)) ; date & time
(require-extension (lib sql/query))
(require-extension (lib sql/jdbc))

(module iasylum/jdbc
  (jdbc/load-drivers jdbc/map-result-set jdbc/get-connection jdbc/for-each-result-set
                     result-set->iterator
                     execute-jdbc-query
                     get-data
                     for-each-data
                     map-each-data
                     jdbc/for-each-triple
                     )

  (define (jdbc/load-drivers)
    (and (j "Class.forName(\"org.postgresql.Driver\");") #t))
  
  (define (jdbc/get-connection url username password)
    (jdbc/load-drivers)
    ;;(jdbc/load-driver "org.postgresql.Driver")
    ;;jdbc:postgresql://host:port/database
    ;; Why the heck the below does not work?
    ;;(jdbc/open-connection "jdbc:postgresql://localhost:5432/dtdata" "dtdata" "dtdata")
    (j "java.sql.DriverManager.getConnection(url,username,password);"
       `(
         (url ,(->jstring url))
         (username ,(->jstring username))
         (password ,(->jstring password))
         )))

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
  (define-generic-java-method set-fetch-size)
  
  (define execute-jdbc-query
    (lambda* (connection query (fetch-size #f))
        (let ((stmt (new-statement connection)))
          (when fetch-size (set-fetch-size stmt (->jint fetch-size)))
          (execute-query  stmt (->jstring query)))))
  
  (define (get-data connection query)
    (define (read-metadata p)
      (let l ((i 1) (m (->number (get-column-count p))))
        (if (> i m) '()
            (cons
             (cons (->scm-object (get-column-name p (->jint i)))
                   (->scm-object (get-column-type-name p (->jint i))))
             (l (+ i 1) m)))))
    (let ((rs (execute-jdbc-query connection query) ))
      (let ((rs-md (read-metadata (get-meta-data rs)))
            (data (iterable->list (result-set->iterator rs) (lambda (v) (->scm-object v)))))
            (cons rs-md data))))

  (define (for-each-data connection query proc)
    (define (read-metadata p)
      (let l ((i 1) (m (->number (get-column-count p))))
        (if (> i m) '()
            (cons
             (cons (->scm-object (get-column-name p (->jint i)))
                   (->scm-object (get-column-type-name p (->jint i))))
             (l (+ i 1) m)))))
    (let ((rs (execute-jdbc-query connection query) ))
      (let ((rs-md (read-metadata (get-meta-data rs))))
            (for-each-iterable (result-set->iterator rs) (lambda (v) (proc (list rs-md (->scm-object v))))))))

  (define (map-each-data connection query proc)
    (define result (list))
    (define (proc v)
      (set! result (cons v result)))
    (for-each-data connection query proc)
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