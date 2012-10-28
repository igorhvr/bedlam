;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/srfi-89))
(require-extension (lib iasylum/match))
;; TODO - Merge all pumps

(module iasylum/iasylum
  (
   hashtable/values
   nyi
   vunless
   to-string
   display-string
   iasylum-write-string
   d
   w
   beanshell-server
   first-n-or-less
   assert
   alist->http-parameters-string
   run-remote-request
   pump-binary
   pump_binary-input-port->character-output-port   
   r r-split r/s r/d r-base
   dp
   smart-compile
   flatten
   rglob
   file->string
   sort
   /*
   find-zipfiles
   get-streams-in-zipfile
   get-streams-in-rarfile
   for-each-row-in-a-spreadsheet-in-a-zipfile
   concurrent-semaphore
   )
  (import hashtable)
  (import file-manipulation)  ;; rglob uses this.

;;  (define* (file->string file-name (max-size #f))
;;    (let ((filesize (file-length file-name)))
;;      (let ((desired-bytes
;;             (if max-size
;;                 (if (> filesize max-size) max-size filesize)
;;                 filesize)))
;;        (let ((result (make-string desired-bytes)))
;;            (call-with-input-file file-name
;;              (lambda (i)
;;                (read-string result 0 desired-bytes i)))
;;            result))))

  (define (file->string fname)
    (->string
     (j "in = new FileReader(filename);
      org.apache.bsf.util.IOUtils.getStringFromReader(in);" `((filename ,(->jstring fname))))))
  
  ;; Sorting routine.
  (define (binarysort cmp-func L)
    (if (null? L) '()
        (traverse (btree L cmp-func))))
  
  (define sort binarysort)
  
  (define (btree L cmp-func)
    (if (= (length L) 1) (leaf (car L))
        (binsert (btree (cdr L) cmp-func) (car L) cmp-func)))
  
  (define (binsert T A cmp-func)     ; insert A into the tree
    (cond  ( (null? T) (leaf A) )        ; insert here
           ( (cmp-func (car T) A) (list (car T) 
                                        (binsert (car (cdr T)) A cmp-func)
                                        (car (cdr (cdr T)))))  ; left subtree 
           ( else (list (car T)
                        (car (cdr T)) 
                        (binsert (car (cdr (cdr T))) A cmp-func))))); right subtree
  
                                        ; add a leaf to the tree (A ()())
  (define (leaf A) (list A '() '()))
  
                                        ; output sorted list by traversing the tree 
  (define (traverse L)   
    (cond ( (null? L) L)
          ( else
            (append (traverse (car (cdr L)))
                    (cons (car L)(traverse (car (cdr (cdr L)))))))))
  
  (define (length L)
    (if (null? L) 0
        (+ 1 (length (cdr L)))))
  
  (define (smart-compile fname)
    (for-each display (list "\n\n(smart-compile \"" fname "\")..."))
    (let ((data-match (irregex-search
                  '(seq (submatch-named file-name (+ any)) ".scm") fname)))
      (let ((fn-prefix (irregex-match-substring data-match 'file-name)))
        (compile-file fname (string-append fn-prefix ".scc")))))
  
  (define hashtable/values
    (lambda (ht)
      (hashtable/map (lambda (k v) v) ht)))

  (define-syntax nyi
    (lambda (x)    
      (syntax-case x ()
        ((_ fname) (syntax (_ fname #t)))
        ((_ fname dummy-value)
         (let* ((symbolic-name (syntax-object->datum (syntax fname)))
                (str-name (symbol->string symbolic-name)))
           (with-syntax ((symbolic-identifier (datum->syntax-object (syntax _) symbolic-name))
                         (str-name (datum->syntax-object (syntax _) str-name)))
             (syntax
              (define symbolic-identifier
                (lambda p
                  (log-debug (string-append "NYI: " str-name " - not yet implemented. Return ") dummy-value)
                  dummy-value)))))))))

  (define-syntax vunless
    (syntax-rules ()
      ((_ test e1 e2 ...)
       (let ((test-value test))
         (if (not test-value)
             (begin e1 e2 ...)
             test-value
             )))))


  
  (define to-string (lambda (f o) (with-output-to-string (lambda () (f o)))))
  
  (define display-string (lambda (o) (to-string display o)))
  
  (define iasylum-write-string (lambda (o) (to-string write o)))

  (define (beanshell-server port)
   (j "bsh.Interpreter i = new bsh.Interpreter();                
       i.set(\"portnum\", port ); i.eval(\"setAccessibility(true)\"); i.eval(\"show()\");
       i.eval(\"server(portnum)\");"
    `((port ,(->jint port)))))
  
  (define (first-n-or-less l n)
    (if (or (eqv? '() l) (= n 0)) '()
        (cons (car l) (first-n-or-less (cdr l) (- n 1)))))
  
  (define (assert v m)
    (unless v (error m)) v)

  (define alist->http-parameters-string
    (lambda (alist)   
      (define me
        (match-lambda
         ;; make sure this also works with regular lists.
         ;; this has to be before to avoid proper lists
         ;; being matched as pairs (which they are).
         (((k   v)  ) (me (list (cons k v))))
         (((k1 v1) . xa) (me `((,k1 . ,v1) . ,xa)))
         
         ;; alist cases.
         (((k .  v)  ) (string-append k "=" v))
         (((k1 . v1) . xa) (string-append k1 "=" v1 "&" (me  xa)))
         ))
      (me alist)))
  

  ;(run-remote-request "www.iasylum.net" 80 test-request "/tmp/tst11.txt")
; (define test-request "GET http://localhost:8080/ HTTP/1.1\nHost: localhost:8080\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.7) Gecko/2009030822 Gentoo Firefox/3.0.7\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\nAccept-Language: en-us,en;q=0.5\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\nCache-Control: max-age=0\nConnection: close\n\n")
;;  "GET /\n\n\n"
;  )


  
    ;; (define (pump-binary i o)
    ;;    (let ((a (read-byte i)))
    ;;      (if (eof-object? a) 'done 
    ;;          (begin (write-byte a o) (pump-binary i o)))))
    (define (pump-binary i o)      
      (define mbuffer (make-buffer 65000))
      (import networking) (import binary-io) (import custom-io)
      (define (loop)
        (let ((a (read-block mbuffer 0 (buffer-length mbuffer) i)))
          (if (eof-object? a) 'done 
              (begin (write-block mbuffer 0 a o)  (loop)))))
      (loop))
  
  (define (run-remote-request host port request url)
    (import networking) (import binary-io)  (import custom-io)
    (call-with-binary-output-file
     url
     (lambda (out-f)
       (let* ((socket (open-tcp-socket host port))
              (out (open-socket-output-port       socket "ISO-8859-1" #t))
              (in  (open-binary-socket-input-port socket)))
         (display request out)
         (flush-output-port out)
         (pump-binary in out-f)
         (close-socket socket)
         ))))
  
  (define (pump_binary-input-port->character-output-port i-b o lock)
    (define i (open-character-input-port i-b))
    
    (define mbuffer (make-string 65000))
    (define (loop)
      (let ((a (read-string mbuffer 0 (string-length mbuffer) i)))
        (if (eof-object? a) 'done 
            (begin
              (mutex/lock! lock)
              (write-string mbuffer 0 a o)
              (mutex/unlock! lock)
              (loop)))))
    (loop))

  (define r
    (lambda* (cmd-string (get-return-code #f))
        (define process-return-code)
        (define result (open-output-string))  
        (define output-lock (mutex/new))

        (set! process-return-code (r-base cmd-string result output-lock result output-lock))
        
        (let ((final-result (get-output-string result)))
          (if get-return-code
              (list process-return-code final-result)
                final-result))))

  (define r-split
    (lambda (cmd-string)
      (define process-return-code)
      (define result-out (open-output-string))      
      (define out-lock (mutex/new))
      (define result-err (open-output-string))      
      (define err-lock (mutex/new))

      (set! process-return-code (r-base cmd-string result-out out-lock result-err err-lock))
        
        (let ((final-result-out (get-output-string result-out))
              (final-result-err (get-output-string result-err)))          
          (list process-return-code final-result-out final-result-err))))

  (define r/d
    (lambda (cmd-string)
      (define l (mutex/new))
      (r-base cmd-string (current-output-port) l (current-output-port) l)
      (void)))
  
  (define r-base
    (lambda* (cmd-string stdout stdout-output-lock stderr stderr-output-lock)
             ((lambda ()
               (define process (spawn-process "bash" (list "-c" cmd-string)))
               (define process-lock (mutex/new))
               (define process-return-code)
               (define stdout-thread)
               (define stderr-thread)
               
               (define (grab-results stream-retriever output-stream stream-lock)
                 (thread/spawn
                  (lambda ()
                    (define processInputStream)
                    (mutex/lock! process-lock)
                    (set! processInputStream (stream-retriever process))
                    (mutex/unlock! process-lock)
                    
                    (pump_binary-input-port->character-output-port processInputStream output-stream stream-lock))))
               
               (set! stdout-thread (grab-results get-process-stdout stdout stdout-output-lock))
               (set! stderr-thread (grab-results get-process-stderr stderr stderr-output-lock))
               
               (set! process-return-code (wait-for-process process))
               
               (thread/join stdout-thread)
               (thread/join stderr-thread)
               process-return-code))))


  (define r/s (lambda p (r (apply string-append p))))

   ;; Defines a parameter.
  (define-syntax dp
    (lambda (x)    
      (syntax-case x ()
        ((_ fname value)
         (let* ((symbolic-name (syntax-object->datum (syntax fname))))
           (with-syntax ((symbolic-identifier (datum->syntax-object (syntax _) symbolic-name)))
             (syntax
              (begin
                (define symbolic-identifier
                  (make-parameter value)))))))
        ((_ fname) (syntax (_ fname #f))))))
  

  
  (define (flatten tree)
    (define result (list))
    ;; This leaves nothing pending on the stack, and doesn't build
    ;; intermediate results that it throws away.
    (define (flatten-element elt)
      (if (list? elt)
          (flatten-a-list elt)
          (set! result (cons elt result))))
    (define (flatten-a-list lst)
      (for-each flatten-element lst))
    
    (if (list? tree)
        (begin
          (flatten-a-list tree)
          (reverse! result))
      tree))
  
  (define (rglob pdirectory)
    (define (do-rglob directory)
      (let ( (d-contents (directory-list directory)) )
        (map (lambda e
               (let ((se (car e)))
                 (let ((full-path (string-append directory "/" se)))
                   (if (file-is-directory? full-path)
                       (do-rglob (string-append full-path))
                       full-path))))
             d-contents)))
    (flatten (do-rglob (string-append pdirectory))))
    
  ;; use like this:
  ;; (/* content ... */ <default-return>)
  ;; or
  ;; (/* content ... */) => #f
  (define-syntax /*
    (syntax-rules (*/)
      ((/* body ... */) #f)
      ((/* body ... */ r) r)))

  
  (define (find-zipfiles location)
    (filter
     (lambda (data)
       (irregex-search (irregex '(w/nocase (seq ".zip" eos))) data))
     (rglob location)))
  
  (define (get-streams-in-zipfile fname)
    (j
     "  import java.io.IOException;
      import java.io.InputStream;
      import java.util.Enumeration;
      import java.util.LinkedList;
      import java.util.zip.ZipEntry;
      import java.util.zip.ZipFile;
      LinkedList result = new LinkedList();
      ZipFile f=new ZipFile(fname);
      Enumeration e = f.entries();
      while(e.hasMoreElements()){
          ZipEntry currE = e.nextElement();
          InputStream is = f.getInputStream(currE);
          Object[] res=new Object[2];
          res[0]=is;
          res[1]=currE.getName();
          result.add(res);
      }
      result;"
     `((fname ,(->jstring fname)))))
  
  
  (define (for-each-row-in-a-spreadsheet-in-a-zipfile table-name proc fname)
    (log-debug table-name "for-each-row-in-a-spreadsheet-in-a-zipfile" "START" fname)
    (with/fc (lambda (error-record error-k)
               (log-error table-name "Failure processing zipfile" fname "Moving to defective files location")
               (log-error table-name "(move-to-failure-location fname) => " (move-to-failure-location fname))
               (print-exception (make-exception error-record error-k)))
             (lambda ()
               (let ((streams+names (get-streams-in-zipfile fname)))    
                 (map
                  (lambda (cstream+name)
                    (let ((ss-name (->string (_1_ cstream+name))))
                      (log-debug table-name "Spreadsheet processing" "START" ss-name)
                      (with/fc (lambda (error-record error-k)
                                 (log-error table-name "Failure processing spreadsheet" ss-name)
                                 (print-exception (make-exception error-record error-k)))
                               (lambda ()
                                 (for-each-excel-sheet-data
                                  (lambda (v) (proc v fname ss-name))
                                  (get-excel-sheet-by-index (get-excel-workbook (_0_ cstream+name))  0))))
                      (log-debug table-name "Spreadsheet processing" "END" ss-name)))
                  (iterable->list streams+names)))
               (file-delete! fname)
               ))
    (log-debug table-name "for-each-row-in-a-spreadsheet-in-a-zipfile" "END" fname))

  ;; Return a stream for each file inside a rarfile. A thread will be launched for each stream and die once it is consumed.
  (define (get-streams-in-rarfile fname)
    (iterable->list
     (j
     "result = new java.util.concurrent.ConcurrentLinkedQueue();
      archive = new com.github.junrar.Archive(new java.io.File(fname));

      extractor(fname, pout, localPosition) {
          run() {
              com.github.junrar.Archive extractorArchive = new com.github.junrar.Archive(new java.io.File(fname));
              Iterator extractorE = extractorArchive.getFileHeaders().iterator();

              com.github.junrar.rarfile.FileHeader extractorHd=null;

              int i=0;
              do {
                  extractorHd = extractorE.next();
              } while (i++<localPosition);              

              extractorArchive.extractFile(extractorHd, pout);
          }
          return this;
      }

      e = archive.getFileHeaders().iterator();
      position=0;

      while(e.hasNext()){
          com.github.junrar.rarfile.FileHeader currE = e.next();

          pinp = new java.io.PipedInputStream();
          pout = new java.io.PipedOutputStream(pinp);

          thread = new Thread( extractor(fname, pout, position) );
          thread.start();

          Object[] res=new Object[2];
          res[0]=pinp;
          res[1]=currE.getFileNameString();
          result.add(res);
          position++;
      }
      result;"
     `((fname ,(->jstring fname))))))
  
  (define d)
  (define w)

  (define concurrent-semaphore
    (lambda* ((initialPermits 0))
        (let ((inner-semaphore (j "new java.util.concurrent.Semaphore(ip);" `((ip ,(->jint initialPermits))))))
          (match-lambda*                              
           [('inc)
            (release inner-semaphore)
            (void)]
           [('inner-semaphore)
            inner-semaphore]
           [()
            (->number (available-permits inner-semaphore)])))))

  (define-generic-java-method release)
  (define-generic-java-method available-permits)
  
  (set! d (let ((m (mutex/new))) (lambda p (mutex/lock! m) (for-each display p) (mutex/unlock! m) (void))))
  (set! w (let ((m (mutex/new))) (lambda p (mutex/lock! m) (for-each write p) (mutex/unlock! m) (void))))
  
  ;; (define d (lambda p (for-each display p)))
)