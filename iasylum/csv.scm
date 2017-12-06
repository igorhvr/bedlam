(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/javascript))

(module iasylum/csv
  (csv->vector csv->list vector->csv)

  (define paparse.js (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/PapaParse-4.3.2/papaparse.min.js"))))
  
  (define (com.eclipsesource.v8.V8Array->vector va)
    (let ((data-results-length (->scm-object (j "va.length();" `((va ,va))))))
      (vector-ec (: i 0 data-results-length)
                 (let ((element (j "va.get(e);" `((e ,(->jint i)) (va ,va)))))
                   (->scm-object element)))))

  (define (vector->com.eclipsesource.v8.V8Array va)
    (let ((v8array (j "new com.eclipsesource.v8.V8Array(engine);" `((engine ,(get-local-javascript-manager))))))
      (vector-ec (: i 0 (vector-length va))
                 (let* ((element (vector-ref va i))
                        (element-to-push (cond [(string? element) (->jstring element)]
                                               [(integer? element) (->jlong element)]
                                               [(vector? element) (vector->com.eclipsesource.v8.V8Array element)]
                                               [else (throw (make-error "Type not suppported yet."))])))
                   (j "v8array.push(element);" `((v8array ,v8array)
                                                 (element ,element-to-push)))))
      v8array))

  (define (csv->vector str)
    (js-v8 (paparse.js))
    (let* ((data-results (js-v8 "tr=Papa.parse(mstr);fres=tr['data'];fres;" `((mstr ,str))))
           (nnn (com.eclipsesource.v8.V8Array->vector data-results)))
      (vector-map (lambda (ignored-index d) (com.eclipsesource.v8.V8Array->vector d)) nnn)))
  
  (define (csv->list str)
    (vector->list_deeply (csv->vector str)))


  ;;
  ;; vec is '#(#(1 2 3) #(3 2 1))
  ;;
  (define (vector->csv vec)
    (js-v8 (paparse.js))
    (->string (js-v8 "Papa.unparse(arr);" `((arr ,(vector->com.eclipsesource.v8.V8Array vec))))))

  )

;; (csv->vector "a,b,c\nc,d,e\nxpto,xuxu,xaxa") (csv->list "a,b,c\nc,d,e\nxpto,xuxu,xaxa")

