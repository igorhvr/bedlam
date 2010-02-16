(require-extension (lib iasylum/jcode))
(require-extension (srfi 19)) ; date & time
(require-extension (lib sql/query))
(require-extension (lib sql/jdbc))
(require-extension (lib sql/postgresql))

(module iasylum/jdbc
  (jdbc/load-drivers jdbc/map-result-set jdbc/get-connection jdbc/for-each-result-set)

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
         ))
    )

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
  
  )