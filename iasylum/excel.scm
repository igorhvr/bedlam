;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (srfi 19)) ; date & time

(module iasylum/excel
  (list->spreadsheet list->spreadsheet-file make-workbook make-sheet add-row add-cell set-cell-value save-wb save-wb-file excel-row->scheme load-excel-sheet-data for-each-excel-sheet-data excel-numeric-date-to-jdate excel-numeric-date-to-date get-excel-workbook get-excel-sheet-by-name get-excel-sheet-by-index
  get-workbook-number-of-sheets excel-spreadsheet->list)

  (define* (list->spreadsheet-file l fn (sheet-name "data"))
    (let ((file-stream
           (j "import java.io.*;
               //FileOutputStream
               listspreadsheetfile_fileOut = new FileOutputStream(filename);
               listspreadsheetfile_fileOut;" `((filename ,(->jstring fn))))))
      (list->spreadsheet l file-stream sheet-name)
      (j "listspreadsheetfile_fileOut.close();" `((fileOut ,file-stream)))))

  
  (define* (list->spreadsheet l stream (sheet-name "data"))
    (let* ((wb (make-workbook))
           (sheet (make-sheet wb sheet-name)))
      (map
       (lambda (v)
         (define row (add-row sheet))
         (map
          (lambda (v)
            (define cell (add-cell row))
            (unless (eqv? v '()) (set-cell-value cell v)))
          v))
       l)
      (save-wb wb stream)))
  
  (define (make-workbook) (j "import org.apache.poi.hssf.usermodel.*;
                              //HSSFWorkbook
                              makeworkbook_wb = new HSSFWorkbook();
                              makeworkbook_wb;"))
  
  (define* (make-sheet wb (sheet-name "data"))
    (j "import org.apache.poi.hssf.usermodel.*;
        //HSSFSheet
        makesheet_sheet = wb.createSheet(sheetname);
        makesheet_sheet" `((wb ,wb) (sheetname ,(->jstring sheet-name)))))
  
  (define (add-row sheet)
    (j
     "import org.apache.poi.hssf.usermodel.*;
                  newPosition = (sheet.getLastRowNum()==0) ? ( (sheet.getPhysicalNumberOfRows()==0) ? 0 : 1 )  : (sheet.getLastRowNum()+1);
                  //HSSFRow
                  addrow_row = sheet.createRow((short)newPosition);
                  addrow_row;
                  " `((sheet ,sheet))))
  
  (define (add-cell row)
    (j
     "import org.apache.poi.hssf.usermodel.*;
                  newPosition = (row.getLastCellNum()==-1) ? 0  : row.getLastCellNum();
                  addcellresult=row.createCell((short)newPosition);
                  addcellresult;
                  " `((row ,row))))
  
  (define (set-cell-value cell value)
    (j "cell.setCellValue(value); " `((cell ,cell) (value ,(->jobject value)))))
  
  (define (save-wb-file wb filename)
    (let ((file-stream
           (j "import java.io.*;
               //FileOutputStream
               savewbfile_fileOut = new FileOutputStream(filename);
               savewbfile_fileOut;" `((filename ,(->jstring filename))))))
      (save-wb wb file-stream)
      (j "fileOut.close();" `((fileOut ,file-stream)))))

  (define (save-wb wb stream)
    (j "wb.write(stream);"
       `((stream ,stream) (wb ,wb))))

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
       //POIFSFileSystem
       getexcelworkbook_fs = new POIFSFileSystem(inputstream);
       //HSSFWorkbook
       getexcelworkbook_wb = new HSSFWorkbook(getexcelworkbook_fs);
       getexcelworkbook_wb;"
     `((inputstream ,input-stream)
       (getexcelworkbook_fs)
       (getexcelworkbook_wb))))

  
  (define (get-workbook-number-of-sheets workbook)
    (j
     " import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       wb.getNumberOfSheets();"
     `((wb ,workbook))))
  

  (define (get-excel-sheet-by-name workbook sheetname)
    (j.
     " import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       //HSSFSheet
       getexcelsheetbyname_sheet = wb.getSheet(sheetname);
       getexcelsheetbyname_sheet;"
     `((wb ,workbook) (sheetname ,(->jstring sheetname)) (getexcelsheetbyname_sheet))))

  (define (get-excel-sheet-by-index workbook index)
    (j
     " import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       //HSSFSheet
       getexcelsheetbyindex_sheet = wb.getSheetAt(index);
       getexcelsheetbyindex_sheet;"
     `((wb ,workbook) (index ,(->jint index)) (getexcelsheetbyindex_sheet))))

;;       (load-excel-sheet-data (get-excel-sheet-by-index (get-excel-workbook (j "new java.io.FileInputStream(fname);" `((fname ,(->jstring fname))))

  (define (excel-spreadsheet->list fname)
    (map
     (lambda (idx) 
       (load-excel-sheet-data (get-excel-sheet-by-index (get-excel-workbook (j "new java.io.FileInputStream(fname);" `((fname ,(->jstring fname)))) ) idx)))
     (iota (->scm-object (get-workbook-number-of-sheets (get-excel-workbook (j "new java.io.FileInputStream(fname);" `((fname ,(->jstring fname)))) ))))))
  
  )
; Example:
;(define excel-data (load-excel-sheet-data (get-excel-sheet-by-index (get-excel-workbook (j "new java.io.FileInputStream(\"/tmp/s.xls\");")) 0)))
;(call-with-serial-output-file "/tmp/excel-data" (lambda (p) (serialize excel-data p)))
; (define excel-data (call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p))))
;(call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p)))
