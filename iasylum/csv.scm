(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/javascript))

(module iasylum/csv
  (csv->vector csv->list)

  (define paparse.js (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/PapaParse-4.3.2/papaparse.min.js"))))
  
  (define (com.eclipsesource.v8.V8Array->vector va)
    (let ((data-results-length (->scm-object (j "va.length();" `((va ,va))))))
      (vector-ec (: i 0 data-results-length)
                 (let ((element (j "va.get(e);" `((e ,(->jint i)) (va ,va)))))
                   (->scm-object element)))))

  (define (csv->vector str)
    (js-v8 (paparse.js))
    (let* ((data-results (js-v8 "tr=Papa.parse(mstr);fres=tr['data'];fres;" `((mstr ,str))))
           (nnn (com.eclipsesource.v8.V8Array->vector data-results)))
      (vector-map (lambda (ignored-index d) (com.eclipsesource.v8.V8Array->vector d)) nnn)))
  
  (define (csv->list str)
    (vector->list_deeply (csv->vector str))))  

;; (csv->vector "a,b,c\nc,d,e\nxpto,xuxu,xaxa") (csv->list "a,b,c\nc,d,e\nxpto,xuxu,xaxa")

