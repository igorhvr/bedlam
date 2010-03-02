;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (srfi 19)) ; date & time

(module iasylum/excel
  (list->spreadsheet list->spreadsheet-file make-workbook make-sheet add-row add-cell set-cell-value save-wb save-wb-file iterable->list for-each-iterable excel-row->scheme load-excel-sheet-data for-each-excel-sheet-data excel-numeric-date-to-jdate excel-numeric-date-to-date get-excel-workbook get-excel-sheet-by-name get-excel-sheet-by-index)

  (define (list->spreadsheet-file l fn)
    (let ((file-stream
           (j "import java.io.*;
               FileOutputStream fileOut = new FileOutputStream(filename);
           fileOut;" `((filename ,(->jstring fn))))))
      (list->spreadsheet l file-stream)
      (j "fileOut.close();" `((fileOut ,file-stream)))))

  
  (define (list->spreadsheet l stream)
    (let* ((wb (make-workbook))
           (sheet (make-sheet wb)))
      (map
       (lambda (v)
         (define row (add-row sheet))
         (map
          (lambda (v)
            (define cell (add-cell row))
            (set-cell-value cell v))
          v))
       l)
      (save-wb wb stream)))
  
  (define (make-workbook) (j "import org.apache.poi.hssf.usermodel.*; HSSFWorkbook wb = new HSSFWorkbook(); wb;"))
  (define (make-sheet wb) (j "import org.apache.poi.hssf.usermodel.*; HSSFSheet sheet = wb.createSheet(\"data\"); sheet" `((wb ,wb))))
  (define (add-row sheet)
    (j
     "import org.apache.poi.hssf.usermodel.*;
                  newPosition = (sheet.getLastRowNum()==0) ? ( (sheet.getPhysicalNumberOfRows()==0) ? 0 : 1 )  : (sheet.getLastRowNum()+1);
                  HSSFRow row = sheet.createRow((short)newPosition);
                  row;
                  " `((sheet ,sheet))))
  (define (add-cell row)
    (j
     "import org.apache.poi.hssf.usermodel.*;
                  newPosition = (row.getLastCellNum()==-1) ? 0  : row.getLastCellNum();
                  row.createCell((short)newPosition)                  
                  " `((row ,row))))
  
  (define (set-cell-value cell value)
    (j "cell.setCellValue(value); " `((cell ,cell) (value ,(->jobject value)))))
  
  (define (save-wb-file wb filename)
    (let ((file-stream
           (j "import java.io.*;
               FileOutputStream fileOut = new FileOutputStream(filename);
           fileOut;" `((filename ,(->jstring filename))))))
      (save-wb wb file-stream)
      (j "fileOut.close();" `((fileOut ,file-stream)))))

  (define (save-wb wb stream)
    (j "wb.write(stream);"
       `((stream ,stream) (wb ,wb))))
  
  (define iterable->list
    (lambda* (o (proc (lambda (p) p)))
      (let ((result (list)))
        (define (p d)
          (set! result (cons (proc d) result)))
        (for-each-iterable o p)
        (reverse result))))

  (define for-each-iterable
    (lambda (o proc)
      (define-generic-java-method iterator)
      (define-generic-java-method has-next)
      (define-generic-java-method next)
      (let ((it (if (->boolean (j "o instanceof java.util.Iterator;"  `((o ,o)))) o (iterator o))))
        (let loop ()
          (let ((more-data (->boolean (has-next it))))
            (if more-data
                (let ((e (next it)))
                  (proc e)
                  (loop))))))))

  (define excel-row->scheme
    (lambda (row)
      (iterable->list
       row
       (lambda (hssfcell)
         (let ((cell-type (->number (j "h.getCellType()" `((h ,hssfcell))))))
           (cond ((= 0 cell-type) ;;CELL_TYPE_NUMERIC
                  (->number (j "h.getNumericCellValue()" `((h ,hssfcell))))) 
                 ((= 1 cell-type) ;;CELL_TYPE_STRING
                  (->string (j "h.toString()" `((h ,hssfcell))))) 
                 ((= 2 cell-type) ;; CELL_TYPE_FORMULA
                  (cons 'formula (->string (j "h.getCellFormula()" `((h ,hssfcell)))))) 
                 ((= 3 cell-type) '()) ;; CELL_TYPE_BLANK
                 ((= 4 cell-type) ;; CELL_TYPE_BOOLEAN
                  (->boolean (j "h.getBooleanCellValue()" `((h ,hssfcell))))) 
                 (else ;;CELL_TYPE_ERROR (5) or anything else
                  (error "Unexpected cell type."))))))))
  
  (define load-excel-sheet-data
    (lambda (sheet)
      (map excel-row->scheme
           (iterable->list sheet))))

  (define for-each-excel-sheet-data
    (lambda (proc sheet)
      (define np (lambda (data) (proc (excel-row->scheme data))))
      (for-each-iterable sheet np)))
  
  
  (define (excel-numeric-date-to-jdate numericdate)
    (j "v=new java.util.GregorianCalendar(1900,0,0);
        v.add(java.util.Calendar.DAY_OF_YEAR,numericdate-1);
        v.getTime();"
       `((numericdate ,(->jint numericdate)))))

  (define (excel-numeric-date-to-date numericdate zone-offset)
    (let ((jd (excel-numeric-date-to-jdate numericdate)))
      (let ((vls (j "new Object[]{jd.getDate(),jd.getMonth(),jd.getYear()};" `((jd ,jd)))))
        (let ((day-of-month (->number (java-array-ref vls 0)))
              (month-of-year (+ 1 (->number (java-array-ref vls 1))))
              (year (+ 1900 (->number (java-array-ref vls 2)))))
          ;;make-date nanosecond second minute hour day month year zone-offset 
          (make-date 0 0 0 0 day-of-month month-of-year year (* zone-offset 3600))))))


;;(excel-numeric-date-to-jdate 39539) --> #<java java.util.Date Tue Apr 01 00:00:00 BRT 2008>
;;(excel-numeric-date-to-jdate 39568) -> #<java java.util.Date Wed Apr 30 00:00:00 BRT 2008>
;;(date->string (excel-numeric-date-to-date 39568))
    


  (define (get-excel-workbook input-stream)
    (j
     "import org.apache.poi.poifs.filesystem.POIFSFileSystem;
       import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       POIFSFileSystem fs = new POIFSFileSystem(inputstream);
       HSSFWorkbook wb = new HSSFWorkbook(fs);
       wb;"
     `((inputstream ,input-stream))))
  
  (define (get-excel-sheet-by-name workbook sheetname)
    (j
     " import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       HSSFSheet sheet = wb.getSheet(sheetname);
       sheet;"
     `((wb ,workbook) (sheetname ,(->jstring sheetname)))))

  (define (get-excel-sheet-by-index workbook index)
    (j
     " import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       HSSFSheet sheet = wb.getSheetAt(index);
       sheet;"
     `((wb ,workbook) (index ,(->jint index)))))

  )
; Example:
;(define excel-data (load-excel-sheet-data (get-excel-sheet-by-index (get-excel-workbook (j "new java.io.FileInputStream(\"/tmp/s.xls\");")) 0)))
;(import java-io)
;(import serial-io)
;(call-with-serial-output-file "/tmp/excel-data" (lambda (p) (serialize excel-data p)))
; (define excel-data (call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p))))
;(call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p)))
