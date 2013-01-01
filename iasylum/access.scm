;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))

(module iasylum/access
  (open-access-db get-table-names get-table get-table-columns open-access-db-java-io-file get-column-name get-column-type)

  (define (open-access-db fname)
    (j "com.healthmarketscience.jackcess.Database.open(new File(fname));" `((fname ,(->jstring fname)))))

  (define (open-access-db-java-io-file f)
    (j "com.healthmarketscience.jackcess.Database.open(java_io_file);" `((java_io_file ,f))))
  
  (define (get-table-names db) (iterable->list (j "db.getTableNames();" `((db ,db))) (lambda (v) (->string v))))
  
  (define (get-table db name)
    (let ((result (j "db.getTable(name);" `((name ,(->jstring name))(db ,db)))))
      (if (java-null? result) #f
          result)))
  
  (define (get-table-columns table) (iterable->list (j "table.getColumns();" `((table ,table)))))

  (define double-type-id) (define long-type-id) (define text-type-id) (define date-type-id) (define memo-type-id)

  (define (get-column-name col) (->string (j "c.getName();" `((c ,col)))))

  (define (get-column-type col)
    (let ((ct (->number (j "c.getType().getValue();" `((c ,col))))))
      (cond ((= ct double-type-id) 'double)
            ((= ct long-type-id) 'long)
            ((= ct text-type-id) 'text)
            ((= ct date-type-id) 'date)
            ((= ct memo-type-id) 'memo)
            (else #f))))

  (set! double-type-id (->number (j "com.healthmarketscience.jackcess.DataType.DOUBLE.getValue();")))
  (set! long-type-id (->number (j "com.healthmarketscience.jackcess.DataType.LONG.getValue();")))
  (set! text-type-id (->number (j "com.healthmarketscience.jackcess.DataType.TEXT.getValue();")))
  (set! date-type-id (->number (j "com.healthmarketscience.jackcess.DataType.SHORT_DATE_TIME.getValue();")))
  (set! memo-type-id (->number (j "com.healthmarketscience.jackcess.DataType.MEMO.getValue();")))
  )