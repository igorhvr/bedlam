;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/srfi-89))
(require-extension (lib iasylum/match))

;; TODO - Merge all pumps

(module iasylum/iasylum
  (
   y-combinator
   hashtable/values
   nyi
   vunless
   each-for
   pam
   to-string
   display-string
   iasylum-write-string
   string-append*
   d reset-d
   w reset-w
   d/n
   beanshell-server
   first-n-or-less
   alist->http-parameters-string
   run-remote-request
   pump-binary
   pump_binary-input-port->character-output-port
   input-port->string
   r r-split r/s r/d r-base
   dp
   smart-compile
   flatten
   rglob
   file->string
   iasylum-sort
   sort
   /*
   find-zipfiles
   get-streams-in-zipfile
   get-streams-in-rarfile
   for-each-row-in-a-spreadsheet-in-a-zipfile
   concurrent-semaphore
   uuid-string
   uuid?
   get-all-jstreams
   create-temporary-directory
   jstream->tmp-file
   string-drop-both
   xor
   ensure-zipped-copy
   vector-binary-search
   function fn function* fn*
   times multiple-values->list
   sleep-milliseconds sleep-seconds sleep-minutes sleep-hours
   current-date-utc current-date-utc-less-one-hour
   list-of-type?
   list-of
   alist?
   pure-alist?
   try-and-if-it-fails-object
   try-and-if-it-fails-or-empty-or-java-null-return-object
   try-and-if-it-throws-object
   dynamic-define
   create-shortcuts
   to-csv-line
   sha256
   decimal->hex
   decimal->maxradix
   vector->list_deeply
   alist-to-url-query-string
   make-parameter*
   start-async-json-engine-with-status-retriever
   add-between-elements
   complete-with-zeroes
   add-between
   add-spaces-between
   get
   avg
   average
   not-buggy-exact->inexact
   )

  ;; This makes scm scripts easier in the eyes of non-schemers.
  (define-syntax function (identifier-syntax lambda))
  (define-syntax function* (identifier-syntax lambda*))
  (define-syntax fn (identifier-syntax lambda))
  (define-syntax fn* (identifier-syntax lambda*))

  ;; Saving some typing...
  (define-syntax def (identifier-syntax define))

  (define y-combinator
    (lambda (h)
      ((lambda (x) (h (lambda (a) ((x x) a))))
       (lambda (x) (h (lambda (a) ((x x) a)))))))

  (define sleep-milliseconds sleep)
  (define sleep-seconds (lambda (t) (sleep-milliseconds (* 1000 t))))
  (define sleep-minutes (lambda (t) (sleep-seconds (* 60 t))))
  (define sleep-hours (lambda (t) (sleep-minutes (* 60 t))))

  (define (current-date-utc)
    (current-date 0))

  (define (current-date-utc-less-one-hour)
    (time-utc->date (subtract-duration (date->time-utc (current-date-utc)) (make-time 'time-duration 0 (* 60 60)))
                    0))

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

  (define (input-port->string input)
    (define i (if (binary-input-port? input)
                  (open-character-input-port input)
                  input))
    (define o (open-output-string))      
    (define mbuffer (make-string 65000))
    (define (loop)
      (let ((a (read-string mbuffer 0 (string-length mbuffer) i)))
        (if (eof-object? a) 'done 
            (begin
              (write-string mbuffer 0 a o)
              (loop)))))
    (loop)
    (get-output-string o))

  (include "mergesort.scm")

  (define iasylum-sort merge-sort)
  (define sort merge-sort)

  (define (smart-compile fname)
    (for-each display (list "\n\n(smart-compile \"" fname "\")..."))
    (let ((data-match (irregex-search
                  '(seq (submatch-named file-name (+ any)) ".scm") fname)))
      (let ((fn-prefix (irregex-match-substring data-match 'file-name)))
        (compile-file fname (string-append fn-prefix ".scc")))))
  
  (define hashtable/values
    (lambda (ht)
      (hashtable/map (lambda (k v) v) ht)))

  ;;
  ;; Example: (nyi function-not-implemented [value])
  ;; it will define "function-not-implemented" as a lambda that will
  ;; return the value "value" (optional) or #t if the value is not defined.
  ;;
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
  
  (define (each-for arguments fn) (for-each fn arguments))
  
  (define (pam arguments fn) (map fn arguments))
  
  (define to-string (lambda (f o) (with-output-to-string (lambda () (f o)))))
  
  (define display-string (lambda (o) (to-string display o)))


  (define (string-append* . p)
    (let ((strings-to-append
           (map (lambda (v)
                  (display-string v)) p)))
      (apply string-append strings-to-append)))
  
  (define iasylum-write-string (lambda (o) (to-string write o)))

  (define (beanshell-server port)
   (j "bsh.Interpreter i = new bsh.Interpreter();                
       i.set(\"portnum\", port ); i.eval(\"setAccessibility(true)\"); i.eval(\"show()\");
       i.eval(\"server(portnum)\");"
    `((port ,(->jint port)))))
  
  (define (first-n-or-less l n)
    (if (or (eqv? '() l) (= n 0)) '()
        (cons (car l) (first-n-or-less (cdr l) (- n 1)))))
  
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
    (iterable->list 
     (j
     "  import java.io.IOException;
      import java.io.InputStream;
      import java.util.Enumeration;
      import java.util.LinkedList;
      import java.util.zip.ZipEntry;
      import java.util.zip.ZipFile;
      Object result = new LinkedList();
      Object f=new ZipFile(fname);
      Object e = f.entries();
      while(e.hasMoreElements()){
          ZipEntry currE = e.nextElement();
          InputStream is = f.getInputStream(currE);
          Object[] res=new Object[2];
          res[0]=is;
          res[1]=currE.getName();
          result.add(res);
      }
      result;"
     `((fname ,(->jstring fname))))))
  
  
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
                  streams+names))
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
              pout.flush();
              pout.close();
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

  (define (get-all-jstreams path+filename)
  (or
   (and-let* ((is-matching (irregex-search (irregex '(w/nocase (: "." (=> extension (or "zip" "rar") eos)))) path+filename))
              (extension (irregex-match-substring is-matching 'extension)))        
          (cond ((string-ci=? "zip" extension)
                 (let ((zip-streams-list (get-streams-in-zipfile path+filename)))
                   (map
                    (lambda (v)                      
                      (list (string-append path+filename "/@zip-compressed@/" (->string (_1_ v)))
                            (_0_ v)))
                    zip-streams-list)))
                ((string-ci=? "rar" extension)
                 (let ((rar-streams-list (get-streams-in-rarfile path+filename)))
                   (map
                    (lambda (v)
                      (list (string-append path+filename "/@rar-compressed@/" (->string (_1_ v)))
                            (_0_ v)))
                    rar-streams-list)))
                (else (error "Bug."))))
   (list (list path+filename (j "new java.io.FileInputStream(fname);" `((fname ,(->jstring path+filename))))))))

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

  (define (create-temporary-directory)
    (->string (j "java.nio.file.Files.createTempDirectory(\"bedlamTempDirectory\").toAbsolutePath().toString();")))

  (define (jstream->tmp-file stream)
  (j "jstreamtofile_result=java.io.File.createTempFile(\"jstream-to-file_\",\".tmp\");
      { 
	jstreamtofile_out = new java.io.FileOutputStream(jstreamtofile_result);
 
	jstreamtofile_read = 0;
	jstreamtofile_bytes = new byte[1024];
 
	while ((jstreamtofile_read = inputstream.read(jstreamtofile_bytes)) != -1) {
		jstreamtofile_out.write(jstreamtofile_bytes, 0, jstreamtofile_read);
	}
 	
	jstreamtofile_out.flush();
	jstreamtofile_out.close();
      }
      jstreamtofile_result;"
     `((inputstream ,stream))))

  (define (string-drop-both s left-n right-n)
    (string-drop (string-drop-right s right-n) left-n))

  (define xor
    (lambda (a b)
      (cond
       (a (if (not b) a #f))
       (else b))))
  
  ;; Ensures there is a .zip version of each file in a given directory.
  (define (ensure-zipped-copy dir)
    (let* ((file-list (sort (rglob dir) string<))
           (file-vector (list->vector file-list))
           (files-without-corresponding-zipfile
            (filter (lambda (v)
                      (and
                       (not (irregex-search (irregex '(w/nocase (: ".zip" eos))) v))
                       (not (vector-binary-search file-vector (string-append v ".zip") string<))))
                    file-list))
           (commands (map (lambda (v)
                            (string-append "zip -9 -r " v ".zip" " " v))
                          files-without-corresponding-zipfile)))
      (for-each
       (lambda (v)
         (d "\n\n\nAbout to run " v "...\n")
         (d "\nResult= " (r/s v)))
       commands)))
  
  ;; If precedes?-operatio returns false for both (precedes? a b) and (precedes? b a)
  ;; this means that a and b are the same element.
  (define vector-binary-search
    (lambda (vec sought . precedes?-operation)
      (let ((precedes? (if (null? precedes?-operation) < (car precedes?-operation))))
        (let loop ((start 0)
                   (stop (- (vector-length vec) 1)))
          (if (< stop start)
              #f
              (let* ((midpoint (quotient (+ start stop) 2))
                     (mid-value (vector-ref vec midpoint)))
                (cond ((precedes? sought mid-value)
                       (loop start (- midpoint 1)))
                      ((precedes? mid-value sought)
                       (loop (+ midpoint 1) stop))
                      (else #t))))))))

  (define-syntax times
    (syntax-rules ()
      ((_ n <code> ...)
       (let ((lambda-set (list-ec (: i n) (lambda () (list ((lambda () <code> ...)))))))
         (apply append (multiple-values->list (apply parallel lambda-set)))))))

  (define-syntax multiple-values->list
    (syntax-rules ()
      ((_ <code> ...)
       (call-with-values (lambda () <code> ...) list))))
  
  (define d)
  (define w)

  (define d/n (lambda p (apply d (flatten (list "\n" p "\n\n")))))

  (define (reset-d)
    (set! d (let ((m (mutex/new))) (lambda p (mutex/lock! m) (for-each display p) (mutex/unlock! m) (void)))))

  (define (reset-w)
    (set! w (let ((m (mutex/new))) (lambda p (mutex/lock! m) (for-each write p) (mutex/unlock! m) (void)))))

  (define (uuid-string) (->string (j "new com.eaio.uuid.UUID().toString();")))
  (define (uuid? uuid) (try-and-if-it-fails-object (#f) (string->juuid uuid)))
  
  ;; Imported from MIT Scheme runtime/list.scm
  (define (list-of-type? object predicate)
    (let loop ((l1 object) (l2 object))
      (if (pair? l1)
          (and (predicate (car l1))
               (let ((l1 (cdr l1)))
                 (and (not (eq? l1 l2))
                      (if (pair? l1)
                          (and (predicate (car l1))
                               (loop (cdr l1) (cdr l2)))
                          (null? l1)))))
          (null? l1))))

  ;; Imported from MIT Scheme runtime/list.scm
  (define (alist? object)
    (list-of-type? object pair?))

  ;; Imported from MIT Scheme runtime/list.scm
  ;; and adapted to check if the elements are pair and not list
  ;; at the same time; this confusion is because a list fits pair? predicate.
  (define (pure-alist? object)
    (list-of-type? object (lambda (o)
                            (and (pair? o) (not (list? o))))))

  ;; Example: (define list-of-string? (list-of string?))
  ;;          (list-of-string? '("bla" "ble")) => #t
  (define-syntax list-of
    (syntax-rules ()
      ((_ predicate)
       (lambda (object)
         (list-of-type? object predicate)))))

  ;;
  ;; This macro attempts to execute the given piece of code, and if it fails
  ;; throwing some kind of error/exception or the "predicate-fail-condition?"
  ;; (a lambda with a single parameter) is true, returns the provided <object>
  ;; (the error itself is dropped).
  ;;
  ;; E.g.: (try-and-if-it-fails-object ('whatever) (/ 5 x)) will
  ;; return 5/x or 'whatever if x is zero.
  ;;
  ;; If you don't put <obj>, e.g.: (try-and-if-it-fails-object () (/ 5 x))
  ;; the result is the error object itself.
  ;;
  (define-syntax define-custom-try-and-if-it-fails-return-object
    (syntax-rules ()
      ((_ macro-name predicate-fail-condition?)
       (begin
         (define-syntax macro-name
           (syntax-rules ()
             ((_ () <code> (... ...))
              (macro-name ('the-error-object) <code> (... ...)))
             ((_ (<obj>) <code> (... ...))
              (begin
                (call-with-current-continuation
                 (lambda (restart-continuation)
                   (let* ((o (delay ((lambda () <obj>))))
                          (force-result (lambda (error error-continuation)
                                          (let ((obj (force o)))
                                            (if (equal? obj 'the-error-object)
                                                (make-exception
                                                 error
                                                 (or error-continuation restart-continuation))
                                                obj)))))
                     (with-failure-continuation
                      (lambda (error error-continuation) (force-result error error-continuation))
                      (lambda ()
                        (let ((result ((lambda () <code> (... ...)))))
                          (let ((final-result
                                 (cond [(predicate-fail-condition? result) (force-result result #f)]
                                       [else result])))
                            final-result)))))))))))))))

  ;; Check error, exception, java null, empty list and #f
  (define-custom-try-and-if-it-fails-return-object
    try-and-if-it-fails-or-empty-or-java-null-return-object
    (lambda (result)
      (or (java-null? result)
          (null? result)
          (eqv? #f result))))

  ;; Check error, exception, java null and #f
  (define-custom-try-and-if-it-fails-return-object
    try-and-if-it-fails-object
    (lambda (result)
      (or (java-null? result)
          (eqv? #f result))))

  ;; Check only for error or exception
  (define-custom-try-and-if-it-fails-return-object
    try-and-if-it-throws-object
    (lambda (result) #f))
  
  ;; (dynamic-define "abc" 123)
  ;; $ abc => 123
  (define-syntax dynamic-define
    (syntax-rules ()
      ((_ non-evaluated-what <body>)
       (let ((what non-evaluated-what))
         (cond ((string? what) (eval `(define ,(string->symbol string) <body>)))
               ((symbol? what) (eval `(define ,what <body>)))
               (else
                (throw
                 (make-error
                  (string-append
                   "Failure to define using handle " (iasylum-wring-string what) " - not a symbol nor a string.")))))))))

  ;; use like this:
  ;; (create-shortcuts (+ -> plus) (- -> minus))
  (define-syntax create-shortcuts
    (syntax-rules (->)
      ((_ (function -> copy) ...)
       (begin
         (define copy function) ...))))

  (define (vector->list_deeply obj)
    (cond [(list? obj) (map (lambda (element)
                              (vector->list_deeply element))
                            obj)]
          [(vector? obj) (vector->list_deeply (vector->list obj))]
          [(pair? obj) (cons (vector->list_deeply (car obj)) (vector->list_deeply (cdr obj)))]
          [else obj]))

  (define (escape-double-quotes str)
    (irregex-replace/all "\"" str "\\\""))

  (define to-csv-line 
    (match-lambda*
     ((single-element) (string-append* "\"" (escape-double-quotes single-element) "\""))
     ((first-element . rest) (string-append (to-csv-line  first-element) " , " (apply to-csv-line  rest)))
     (anything (error "Invalid parameter to to-csv-line " anything))))

  (define (sha256 string)
    (->string (j 
               "md = java.security.MessageDigest.getInstance(\"SHA-256\");
                md.update(input.getBytes(\"UTF-8\"));
                digest = md.digest();
                new java.math.BigInteger(1, digest).toString(16);" 
                 `((input ,(->jstring string))))))

  (define (decimal->hex decimal)
    (->string (j "new java.math.BigInteger(input).toString(16);"
                 `((input ,(->jstring (number->string decimal)))))))

  (define (decimal->maxradix decimal)
    (->string (j "new java.math.BigInteger(input).toString(java.lang.Character.MAX_RADIX);"
                 `((input ,(->jstring (number->string decimal)))))))

  (define (alist-to-url-query-string raw-alist)
    (define join-parameters
      (match-lambda
       ((element) element)
       ((first-element . rest) (string-append* first-element "&" (join-parameters rest)))))
    
    (let* ((alist (pam raw-alist (lambda (v) (match-let (((key . value) v)) `(,(-to_ key) . ,(->string (j "java.net.URLEncoder.encode(v, \"ISO-8859-1\");" `((v ,(->jstring value))))))))))         
           (individual-parameters (map (lambda (v) 
                                         (match-let (((key . value) v)) (string-append* key "=" value))) alist)))
      (join-parameters individual-parameters)))


  ; Generates parameters that work and are safe across threads.
  (define (make-parameter* init)
    (let ((storage (j "new java.util.concurrent.ConcurrentHashMap();")))
      (j "tmap.put(\"SINGLEKEY\", value);" `((value ,(java-wrap init)) (tmap ,storage)))
      ((lambda ()
         (define calculate-result
           (lambda (value)
             (if (not value)
                 (safe-java-unwrap (j "tmap.get(\"SINGLEKEY\");" `((tmap ,storage))))
                 (let ((result (calculate-result #f)))
                   (j "tmap.put(\"SINGLEKEY\", value);" `((value ,(java-wrap value)) (tmap ,storage)))
                   result))))
         (case-lambda
           (() (calculate-result #f))
           ((init) (calculate-result init)))))))
  
  (define (start-async-json-engine-with-status-retriever engine-thunk should-stop?-thunk seconds-between-executions)
    (define last-execution-result (make-parameter* '#((status . "NEVER_EXECUTED"))))
    (define last-execution-when (make-parameter* #f))
    
    (thread/spawn
     (lambda ()     
       (let loop ()
         (if (should-stop?-thunk)
             'stopped
             (begin
               (last-execution-result
                (let ((rv (try-and-if-it-throws-object () (engine-thunk))))
                  (if (exception? rv)
                      `#((status . ,(string-append "Error: "
                                                   (or (error-message (exception-error rv)) "UnspecifiedError"))))
                      rv)))
               (last-execution-when (current-time time-utc))
               (sleep (* 1000 seconds-between-executions))
               (loop))))))
    
    (lambda ()
      (let* ((when (last-execution-when))
             (result (last-execution-result))
             (time-elapsed
              (or (and when (time-second (time-difference (current-time time-utc) when))) -1)))
        
        (list->vector
         (append
          (vector->list result)
          `(
            (time-elapsed-since-last-updated-seconds . ,time-elapsed)
            (last-updated . ,(if when (date->string (time-utc->date when 0) "~4") "NEVER"))))))))

(define subtract-dates
  (match-lambda*
   (()
    (string-append* "Sample usage: " (iasylum-write-string `(subtract-dates "2014-11-05T10:21:00Z" "2014-11-04T15:15:00Z"))))
   (( (? string? later) (? string? earlier))
    (time-difference (date->time-utc (string->date later "~Y-~m-~dT~k:~M:~S~z"))  (date->time-utc (string->date earlier "~Y-~m-~dT~k:~M:~S~z"))))
   (( (? date? later) (? date? earlier))
    (time-difference (date->time-utc later)  (date->time-utc earlier)))))
  
  (define (add-between-elements what list-of-elements)
    (if (<= (length list-of-elements) 1)
        list-of-elements
        (append (list (car list-of-elements) what)
                (add-between-elements what (cdr list-of-elements)))))

  ;;
  ;; (complete-with-zeroes "56" 7)
  ;; => "0000056"
  ;;
  (define (complete-with-zeroes string zeroes)
    (string-pad string zeroes #\0))

  ;;
  ;; (add-between "," 1 2 3) => "1,2,3"
  ;;  
  (define add-between
    (match-lambda*
     ((what) "")
     ((what single-element) (string-append* single-element))
     ((what first-element . rest) (string-append* first-element what (apply add-between (cons what rest))))
     (anything (error "Invalid parameter to to-csv-line " anything))))

  ;;
  ;; (add-spaces-between 1 2 3) => "1 2 3"
  ;;
  (define add-spaces-between
    (lambda params
      (apply add-between (append (list " ") params))))

  ;;
  ;; (define alist '((a . 1) (b . 3)))
  ;; (get 'b alist) => 3
  ;; (get 'c alist) => #f
  ;; (get 'c alist 123) => 123
  ;;
  (define* (get property alist (default-value #f))
    (or (let ((pair (assoc property alist)))
          (and pair (cdr pair)))
        default-value))

  (define (avg . numbers)  
    (/ (apply + numbers) (length numbers)))

  ;;
  ;; There is a bug on SISC that when you try to format numbers like
  ;; 282457054241909808547136319475365783206389579211833163126981837752475980274261722819705041556589987183902148854677474937212682452658934957606996941102010807245608033464985202685067640880869257553850446454726752603027573191148813419329621578462766074442169646855371987045338851099884530056618741223228936994941553896821661753734605673855194130883776944673309108647011130597/1001509038269881690964475821585963181094481778952714378880702564640950073238802107511786471142177771710461016835321271186203812012260492331963455165457486865903556624479070974292666405020019403147541868610286679558199714122163490068271002294617295514046193024960538970649724427145698755641672350937606977382278465505350339120293706504726356045515482771243984000000000000000
  ;; the format fn throws Error in round: <java.lang.NumberFormatException>: Infinite or NaN
  ;; because this huge number converted to inexact number is nan.0 (using exact->inexact).
  ;;
  ;; Now, if you want to format huge numbers like this, use (format "~0,8F" (not-buggy-exact->inexact <huge-number>))
  ;; instead (format "~0,8F" <huge-number>) directly.
  ;;
  (define (not-buggy-exact->inexact number)
    (string->number (->string (number->jbigdecimal number))))

  (create-shortcuts (avg -> average))

  (define-generic-java-method release)
  (define-generic-java-method available-permits)
  
  (reset-d)
  (reset-w)  
)
