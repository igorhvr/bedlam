;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))

(module iasylum/excel
  (list->spreadsheet make-workbook make-sheet add-row add-cell set-cell-value save-wb iterable->list load-excel-sheet-data get-excel-sheet)

  (define (list->spreadsheet l fn)
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
      (save-wb wb fn)))
  
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
  
  (define (save-wb wb filename)
    (j "import java.io.*;
      FileOutputStream fileOut = new FileOutputStream(filename);
      wb.write(fileOut);
      fileOut.close();" `((filename ,(->jstring filename)) (wb ,wb))))
  
  
  (define iterable->list
    (lambda* (o (proc (lambda (p) p)))
             (define-generic-java-method iterator)
             (define-generic-java-method has-next)
             (define-generic-java-method next)
             (let ((it (if (->boolean (j "o instanceof java.util.Iterator;"  `((o ,o)))) o (iterator o))))
               (let loop ()
                 (let ((more-data (->boolean (has-next it))))
                   (if more-data
                       (let ((e (next it)))
                         (cons (proc e) (loop)))
                       '()))))))
  
  (define load-excel-sheet-data
    (lambda (sheet)
      (map (lambda (row)
             (iterable->list row (lambda (hssfcell)
                                   (->string (j "h.toString()" `((h ,hssfcell)))))))
           (iterable->list sheet))))
  
  
  (define (get-excel-sheet input-stream sheetname)
    (j
     "import org.apache.poi.poifs.filesystem.POIFSFileSystem;
       import org.apache.poi.hssf.usermodel.HSSFWorkbook;
       import org.apache.poi.hssf.usermodel.HSSFSheet;
       POIFSFileSystem fs = new POIFSFileSystem(inputstream);
       HSSFWorkbook wb = new HSSFWorkbook(fs);
       HSSFSheet sheet = wb.getSheet(sheetname);
       sheet;"
     `((inputstream ,input-stream) (sheetname ,(->jstring sheetname)))))

  )
;(define excel-data (load-excel-sheet-data (get-excel-sheet (j "new java.io.FileInputStream(\"/home/igorhvr/p.xls\");"))))
;(import java-io)
;(import serial-io)
;(call-with-serial-output-file "/tmp/excel-data" (lambda (p) (serialize excel-data p)))
; (define excel-data (call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p))))
;(call-with-serial-input-file "/tmp/excel-data" (lambda (p) (deserialize p)))
