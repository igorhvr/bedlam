;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/srfi-89))
(require-extension (lib iasylum/match))
(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/assert))

;; TODO - Merge all pumps

(module iasylum/iasylum
  (
   y-combinator
   hashtable/values
   nyi
   vunless
   each-for
   pam filter-pam
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
   watched-parallel
   debug-watched-parallel
   current-thread-name current-thread-id
   set-current-thread-name!
   thread/spawn*
   watched-thread/spawn
   r r-split r/s r/d r-base ; it is dangerous, improper handling of string arguments creates security liabilities. see safe-bash-run
   safe-bash-run ; also dangerous - will deadlock with large stdout outputs!
   safe-run ; child of r and safe-bash-run -> Use this one! Hopefully the last.
   secure-file-delete!
   dp
   smart-compile
   flatten
   rglob
   file->string string->file
   iasylum-sort
   sort
   avector-lexicographic-sort
   alist-lexicographic-sort
   /*
   find-zipfiles
   get-streams-in-zipfile
   get-streams-in-rarfile
   for-each-row-in-a-spreadsheet-in-a-zipfile
   make-semaphore
   uuid-string
   uuid?
   get-all-jstreams
   create-temporary-directory
   string-drop-both
   xor
   xnor
   ensure-zipped-copy
   vector-binary-search
   function fn function* fn*
   def
   times multiple-values->list
   sleep-milliseconds sleep-seconds sleep-minutes sleep-hours sleep-forever
   has-no-duplicates?
   list-of-type?
   list-of
   alist?
   pure-alist?
   split-list-in-groups
   string-stack-trace
   try-and-if-it-fails-object
   try-and-if-it-fails-or-empty-or-java-null-return-object
   try-and-if-it-throws-object
   retry-blocking-until-succeed
   try-with-exponential-backoff
   dynamic-define
   create-shortcuts
   to-csv-line
   sha256
   sha256+
   sha512
   hex->decimal
   decimal->hex
   decimal->maxradix
   deep-map
   vector->list_deeply
   list->vector_deeply
   alist-to-url-query-string
   make-parameter*
   make-expiring-parameter*
   make-thunk-unpacking-parameter*
   make-blocking-parameter*
   subtract-dates
   start-async-json-engine-with-status-retriever
   complete-with-zeroes
   add-between
   add-between-list
   add-spaces-between
   get
   avg average
   nan?
   not-buggy-exact->inexact
   apply*
   append-lists-when
   let-parallel map-parallel
   atomic-execution
   select-sublist
   shuffle
   make-future
   only
   sum-alist
   base64-encode base64-decode
   group-by-key-and-apply
   get-env
   validate-cpf
   get-relative-time
   get-relative-duration
   pretty-print-to-string
   split-string-comma
   split-string
   format-message
   clear-string
   adjust-date-timezone
   simulate-error
   zip-with-password
   select
   url?
   url->tmp-file
   tmp-file
   html-encode
   shutdown panic
   create-counter
   ->
   ->>
   ellipsis...
   week-number-iso8601
   week-based-year-iso8601
   week-iso8601-string-representation
   get-last-week-iso8601
   get-date-interval-of-week
   eval-string eval/string
   amb
   amb-fail
   amb-require
   amb-safe-context
   amb-possibility-list
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
  (define sleep-forever (lambda ignored (let v () (sleep-hours 1000000) (v))))
  
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
      org.apache.bsf.util.IOUtils.getStringFromReader(in);" `((filename ,(->jstring fname))(in)))))

  (define* (string->file str (prefix: prefix "jstream-to-file") (suffix: suffix ".tmp"))
    (let ((java-io-file (string->java.io.File str 'prefix: prefix 'suffix: suffix)))
      (->string (j "sff.getAbsolutePath();" `((sff ,java-io-file))))))

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

  ;; an "avector" is a set of key-values relationships
  ;; (created using (cons key value)), and very similar
  ;; to an alist, except that the enclosing structure
  ;; is a vector.
  (define (avector-lexicographic-sort v)
    (list->vector
     (sort (lambda (e1 e2)
             (string< (display-string (car e1)) (display-string (car e2))))
           (vector->list v))))

  (define (alist-lexicographic-sort v)
    (sort (lambda (e1 e2) (string< (display-string (car e1)) (display-string (car e2)))) v))
          
  (define (smart-compile fname)
    (for-each display (list "\n(smart-compile \"" fname "\") ..."))
    (let ((data-match (irregex-search
                  '(seq (submatch-named file-name (+ any)) ".scm") fname)))
      (let ((fn-prefix (irregex-match-substring data-match 'file-name)))
        (let ((destination-fn (string-append fn-prefix ".scc")))
          (with/fc (lambda (error-record error-k)
                     (file-delete! destination-fn)
                     (throw error-record error-k))
                   (lambda () 
                     (compile-file fname destination-fn)))))))

  
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
        ((_) (syntax (throw (make-error "Not yet implemented and no dummy value provided."))))
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

  (define (filter-pam arguments fn) (filter-map fn arguments))
  
  (define to-string (lambda (f o) (with-output-to-string (lambda () (f o)))))
  
  (define display-string (lambda (o) (to-string display o)))


  (define (string-append* . p)
    (let ((strings-to-append
           (map (lambda (v)
                  (display-string v)) p)))
      (apply string-append strings-to-append)))
  
  (define iasylum-write-string (lambda (o) (to-string write o)))

  (define (beanshell-server port)
   (j "i = new bsh.Interpreter();                
       i.set(\"portnum\", port ); i.eval(\"setAccessibility(true)\"); i.eval(\"show()\");
       i.eval(\"server(portnum)\");"
    `((port ,(->jint port))(i))))
  
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

  (define (syserr-log p) (j "System.err.print(m); System.err.flush();" `((m ,(->jobject p)))))

  (define (current-thread-name) (->string (j "Thread.currentThread().getName();")))
  (define (set-current-thread-name! new-thread-name)
    (j "Thread.currentThread().setName(ctsnname);" `((ctsnname ,(->jstring new-thread-name))))
    new-thread-name)
  (define (current-thread-id) (->string (j "Thread.currentThread().getId();")))

  (define debug-standard-thread-error-handler
    (lambda (error error-continuation)
      (map syserr-log `("Error - will stop thread - " ,(current-thread-name) " - details saved at " ,(save-to-somewhere (list error error-continuation)) " - " ,error ,error-continuation "\n"))
      (print-stack-trace error-continuation)))

  (define standard-thread-error-handler
    (lambda (error error-continuation)
      (map syserr-log `("Error - will stop thread "  ,(current-thread-name) ": " ,error ,error-continuation "\n"))
      (print-stack-trace error-continuation)))
  
  (define watched-parallel
    (lambda fn-set
      (apply parallel (pam fn-set (lambda (fn) (delay (with/fc standard-thread-error-handler fn)))))))
  
  (define debug-watched-parallel
    (lambda fn-set
      (apply parallel (pam fn-set (lambda (fn) (delay (with/fc debug-standard-thread-error-handler fn)))))))

  (define thread/spawn*
    (lambda* ((error-handler: error-handler standard-thread-error-handler) (thread-name: thread-name #f) thunk)
             (watched-thread/spawn
              'error-handler: error-handler
              'thread-name: thread-name
              thunk)))

  (define watched-thread/spawn
    (lambda* ((error-handler: error-handler standard-thread-error-handler) (thread-name: thread-name #f) p)
        (assert (procedure? p) (or (not thread-name) (string? thread-name)))
        (thread/spawn
         (lambda ()
           (when thread-name (set-current-thread-name! thread-name))
           (with/fc
            error-handler
            p)))))

  (define debug-watched-thread/spawn
    (lambda* ((error-handler: error-handler debug-standard-thread-error-handler) (thread-name: thread-name #f) p)
        (assert (procedure? p) (or (not thread-name) (string? thread-name)))
        (thread/spawn
         (lambda ()
           (when thread-name (set-current-thread-name! thread-name))
           (with/fc
            error-handler
            p)))))

  (define r
    (lambda* (cmd-string (get-return-code #f))
        (define process-return-code)
        (define result (open-output-string))  
        (define output-lock (mutex/new))

        (set! process-return-code (r-base 'cmd-string: cmd-string result output-lock result output-lock))
        
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

      (set! process-return-code (r-base 'cmd-string: cmd-string result-out out-lock result-err err-lock))
        
        (let ((final-result-out (get-output-string result-out))
              (final-result-err (get-output-string result-err)))          
          (list process-return-code final-result-out final-result-err))))

  (define r/d
    (lambda (cmd-string)
      (define l (mutex/new))
      (r-base 'cmd-string: cmd-string (current-output-port) l (current-output-port) l)
      (void)))
  
  (define r-base
    (lambda* ((cmd-string: cmd-string #f) (cmd-list: cmd-list #f)
              stdout stdout-output-lock stderr stderr-output-lock)
             (assert (or cmd-string cmd-list))
             ((lambda ()
               (define process (or (and cmd-string (spawn-process "bash" (list "-c" cmd-string)))
                                   (and cmd-list (spawn-process (car cmd-list) (cdr cmd-list)))))

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

  (define safe-run
    (lambda command-and-args
      (define process-return-code)
      (define result (open-output-string))
      (define output-lock (mutex/new))

      (set! process-return-code (r-base 'cmd-list: command-and-args result output-lock result output-lock))

      (let ((final-result (get-output-string result)))
        final-result)))

  (define (secure-file-delete! fn)
    (let ((return-value-from-secure-erase
           (try-and-if-it-throws-object (#f)
                                        (r-base 'cmd-list: `(,"srm" ,fn) (open-output-string) (mutex/new)
                                                                         (open-output-string) (mutex/new)))))
      (if (and return-value-from-secure-erase (= 0 return-value-from-secure-erase))
          #t
          (begin
            (log-warn "Secure erase of file using srm from the secure_deletion toolkit failed." "Reverting to unsafe/regular erasing. Affected file: " fn)
            (file-delete! fn)))))

  ;;
  ;; This is safer than r-base and derived like r/s and r/d. This fn filters a lot
  ;; of code injections possibilities.
  ;;
  ;; If any command (first argument) can be from an user input, it still necessary to filter the user
  ;; input. If you can avoid to use this, like calling a directly library in Java, please do it.
  ;;
  ;; Use like this: (safe-bash-run "ls" "-lath")
  ;;
  ;; (safe-bash-run <command> <arg1> <arg2> ...)
  ;;
  (define safe-bash-run
    (lambda command-and-args
      (->string
       (j "args = java.util.Arrays.copyOf(cmdparams, cmdparams.length, String[].class);
           // System.out.println(java.util.Arrays.deepToString(args));
           pb = new ProcessBuilder(args);
           pb.redirectErrorStream(true);
           p = pb.start();
           errcode = p.waitFor();
           reader = new java.io.BufferedReader(
                   new java.io.InputStreamReader(p.getInputStream()));

           builder = new StringBuilder();
           line = null;
           while ( (line = reader.readLine()) != null) {
               builder.append(line);
               builder.append(System.lineSeparator());
           }
           result = builder.toString();
           return result;"
          `((args)(pb)(p)(errcode)(reader)(builder)(line)(result)
            (cmdparams ,(jlist->jarray (->jobject (map (lambda (value)
                                                         (->jstring value))
                                                       command-and-args)))))))))
  
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

  (define make-semaphore
    (lambda* ((initialPermits 0))
        (let ((inner-semaphore (j "new java.util.concurrent.Semaphore(ip,true);" `((ip ,(->jint initialPermits))))))
          (match-lambda*
           [('release)
            (release inner-semaphore)
            (void)]
           [('release-all-blocked)
            (let loop ()
              ;; FIXXXME May leave one more permit than it should if
              ;; a thread is blocked because of hasQueuedThreads behavior.
              (when (->boolean (has-queued-threads inner-semaphore))
                (release inner-semaphore)
                (loop)))
            (void)]
           [('acquire)
            (acquire inner-semaphore)
            (void)]
           [('inner-semaphore)
            inner-semaphore]
           [()
            (->number (available-permits inner-semaphore)])))))

  (define (create-temporary-directory)
    (->string (j "java.nio.file.Files.createTempDirectory(\"bedlamTempDirectory\").toAbsolutePath().toString();")))

  (define (string-drop-both s left-n right-n)
    (string-drop (string-drop-right s right-n) left-n))

  (define xor
    (lambda (a b)
      (cond
       (a (if (not b) a #f))
       (else b))))

  (define xnor
    (lambda (a b)
      (not (xor a b))))

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

  ;;
  ;; (times 2 (random))
  ;; => (754466479064 851410907158)
  ;;
  (define-syntax times
    (syntax-rules ()
      ((_ n <code> ...)
       (let ((lambda-set (list-ec (: i n) (lambda () (list ((lambda () <code> ...)))))))
         (apply append (multiple-values->list (apply watched-parallel lambda-set)))))))

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

  (define* (has-no-duplicates? l (comparison-proc equal?)) (and (= (length l) (length (delete-duplicates l comparison-proc))) l))

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

  (define* (split-list-in-groups (group-size: group-size) dtn)
    (let loop ((data dtn))
      (let* ((size-to-consider (min group-size (length data)))
             (group (take data size-to-consider))
             (rest (drop data size-to-consider)))
        (cons group (if (null? rest) '() (loop rest))))))

  ;; Example: (define list-of-string? (list-of string?))
  ;;          (list-of-string? '("bla" "ble")) => #t
  (define-syntax list-of
    (syntax-rules ()
      ((_ predicate)
       (lambda (object)
         (list-of-type? object predicate)))))

  (define* (string-stack-trace (continuation #f))
    (with-output-to-string (lambda () (print-stack-trace (or continuation (call-with-current-continuation (lambda (k) k)))))))

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
                      (lambda (error error-continuation)
                        (log-debug "Probably this error is being purposely ignored:" error)
                        (print-stack-trace error-continuation)
                        (force-result error error-continuation))
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

  ;;
  ;; Block until the thunk execution succeed. The default is sleep 1 sec
  ;; between each retry.
  ;;
  ;; Example usage: (retry-blocking-until-succeed 'retry-n-times: 10
  ;;                                              (lambda ()
  ;;                                                (display "retrying...")
  ;;                                                (/ 2 0)))
  ;;
  (define* (retry-blocking-until-succeed (retry-n-times: retry-n-times)
                                         (sleep-between-retry-ms: sleep-between-retry-ms 1000)
                                         (on-error: on-error-thunk (lambda a a))
                                         thunk)
    (let retry ((retry-counter 0))
      (if (>= retry-counter retry-n-times)
          (thunk) ; last attempt
          (or (try-and-if-it-throws-object (#f) (thunk))
              (begin (on-error-thunk)
                     (sleep sleep-between-retry-ms)
                     (retry (+ retry-counter 1)))))))

  (define try-with-exponential-backoff
    (lambda* ((action: action #f) (action-description: action-description (iasylum-write-string action))
         (initial-interval-millis: initial-interval-millis 50)
         (max-elapsed-time-millis: max-elapsed-time-millis 2147483647) ;; Aprox 24 days
         (max-interval-millis: max-interval-millis 30000)
         (log-error: log-error log-error) (log-info: log-info log-info) (log-debug: log-debug log-debug)
         (add-jitter: add-jitter #t) ;; See https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
         (abort-retries-and-throw?: abort-retries-and-throw? (lambda (error error-continuation) #f))
         (success-after-at-least-one-retry-hook: success-after-at-least-one-retry-hook
                                                 (lambda* ((elapsed-time-milliseconds: t) (total-retries: n) (action-description: d) (return-value: rt))
                                                          (log-info "RETRY SUCCESS. total-retries: " n " elapsed-time-milliseconds: " t " action-description: " action-description)))
         (clean-success-hook: clean-success-hook
                                                 (lambda* ((elapsed-time-milliseconds: t) (total-retries: n) (action-description: d) (return-value: rt))
                                                          (log-debug "CLEAN SUCCESS. total-retries: " n " elapsed-time-milliseconds: " t " action-description: " action-description))))
        (when (not action)
          (d/n "Mandatory parameter action not provided. Sample usage: " "(try-with-exponential-backoff 'action: (lambda () (/ 2 3) (/ 2 0)) 'action-description: \"Let's try to divide by zero.\" 'initial-interval-millis: 50 'max-interval-millis: 200 'max-elapsed-time-millis: 1000 'log-error: d/n 'abort-retries-and-throw?: (lambda (error error-continuation) #f))")
          (error "No action provided for trying with exponential backoff."))
        (let ((auto-retry (j "new com.google.api.client.util.ExponentialBackOff.Builder().
                            setInitialIntervalMillis(iim).setMaxElapsedTimeMillis(metm).
                            setMaxIntervalMillis(mim).setMultiplier(1.5).setRandomizationFactor(0.5).build();"
                             `((iim ,(->jint initial-interval-millis))
                               (metm ,(->jint max-elapsed-time-millis))
                               (mim ,(->jint max-interval-millis)))))
              (total-retries (make-parameter* 0)))
          (let try-again ()
            (with-failure-continuation
             (lambda (error error-continuation)
               (let ((abort-retries-and-throw-result (abort-retries-and-throw? error error-continuation)))
                 (when abort-retries-and-throw-result
                   (throw (make-nested-error
                           (make-error
                            (string-append*
                             "abort-retries-and-throw? predicate returned "
                             (iasylum-write-string abort-retries-and-throw-result)))
                           error error-continuation))))
               (let* ((ms-to-wait-orig (->scm-object (j "backoff.getCurrentIntervalMillis();" `((backoff ,auto-retry)))))
                      (ms-to-wait (if add-jitter
                                      ;; When jitter is enabled the time to wait
                                      ;; will be between 50% below and 50% above the ms value.
                                      (let ((circle-around-value ms-to-wait-orig))
                                        (exact-ceiling (+ (/ circle-around-value 2) (random circle-around-value))))
                                      ms-to-wait-orig))
                      (ms-elapsed (->scm-object (j "backoff.getElapsedTimeMillis();" `((backoff ,auto-retry)))))
                      (ms-max (->scm-object (j "backoff.getMaxElapsedTimeMillis();" `((backoff ,auto-retry))))))
                 (log-error
                  (format
                   "Error when trying to perform the following action: ~a . I'll try again after ~a ms. Elapsed: ~a ms, Max: ~a ms."
                   action-description ms-to-wait ms-elapsed ms-max) error)
                 (sleep ms-to-wait)
                 (let ((next-backoff (->number (j "backoff.nextBackOffMillis();" `((backoff ,auto-retry))))))
                   (if (= -1 next-backoff)
                       (throw
                        (make-nested-error
                         (make-error
                          "max-elapsed-time-millis reached while performing retries with exponential backoff strategy. Giving up.")
                         error error-continuation))
                       (begin
                         (total-retries (+ 1 (total-retries)))
                         (try-again))))))
             (lambda ()
               (let ((final-result (action)))
                 (when (and success-after-at-least-one-retry-hook (> (total-retries) 0))
                   (success-after-at-least-one-retry-hook
                    'elapsed-time-milliseconds:
                      (->scm-object (j "backoff.getElapsedTimeMillis();" `((backoff ,auto-retry))))
                      'total-retries: (total-retries)
                      'action-description: action-description
                      'return-value: final-result))
                 (when (and clean-success-hook (= (total-retries) 0))
                   (clean-success-hook
                    'elapsed-time-milliseconds:
                      (->scm-object (j "backoff.getElapsedTimeMillis();" `((backoff ,auto-retry))))
                      'total-retries: (total-retries)
                      'action-description: action-description
                      'return-value: final-result))
                 final-result)))))))

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

  (define (deep-map f l)
    (define (deep x)
      (cond ((null? x) x)
            ((vector? x) (list->vector (deep-map f (vector->list x))))
            ((list? x) (map deep x))
            ((pair? x) (cons (deep (car x)) (deep (cdr x))))
            (else (f x))))
    (map deep l))
  
  (define (vector->list_deeply obj)
    (cond [(list? obj) (map (lambda (element)
                              (vector->list_deeply element))
                            obj)]
          [(vector? obj) (vector->list_deeply (vector->list obj))]
          [(pair? obj) (cons (vector->list_deeply (car obj)) (vector->list_deeply (cdr obj)))]
          [else obj]))
  
  (define (list->vector_deeply obj)
    (cond [(vector? obj) (list->vector (map (lambda (element)
                                              (list->vector_deeply element))
                                            (vector->list obj)))]
          [(list? obj) (list->vector_deeply (list->vector obj))]
          [(pair? obj) (list->vector_deeply (list (car obj) (cdr obj)))]
          [else obj]))

  (define (escape-double-quotes str)
    (irregex-replace/all "\"" str "\\\""))

  (define to-csv-line 
    (match-lambda*
     ((single-element) (string-append* "\"" (escape-double-quotes (display-string single-element)) "\""))
     ((first-element . rest) (string-append (to-csv-line  first-element) " , " (apply to-csv-line  rest)))
     (anything (error "Invalid parameter to to-csv-line " anything))))

  (define sha256+
    (lambda elements
      (sha256 (apply add-between (cons ":" elements)))))

  (define (sha256 string)
    (->string (j
               "md = java.security.MessageDigest.getInstance(\"SHA-256\");
                md.update(input.getBytes(\"UTF-8\"));
                digest = md.digest();
                return String.format(\"%064x\", new java.math.BigInteger(1, digest));"
                 `((input ,(->jstring string))))))

  (define (sha512 string)
    (->string (j
               "md = java.security.MessageDigest.getInstance(\"SHA-512\");
                md.update(input.getBytes(\"UTF-8\"));
                digest = md.digest();
                return String.format(\"%0128x\", new java.math.BigInteger(1, digest));"
                 `((input ,(->jstring string))))))

  (define (hex->decimal hex)
    (string->number (->string (j "new java.math.BigInteger(hex, 16).toString();" `((hex ,(->jstring hex)))))))
  
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
  (define make-parameter*
    (match-lambda*
     [(init)
      (let ((storage (j "new java.util.concurrent.ConcurrentHashMap();")))
        (make-parameter-with-storage* init storage))]
     [()
      (let ((storage (j "new java.util.concurrent.ConcurrentHashMap();")))
        (make-parameter-with-storage* 'throw-error-upon-void: #t (void) storage))]
     ))

  ; Generates expiring parameters that work and are safe across threads.
  (define* (make-expiring-parameter* (expiration-seconds: expiration-seconds 60) (initial-value: initial-value #f))
    (let ((storage  (j "com.google.common.cache.CacheBuilder.newBuilder()
                     .maximumSize(1)
                     .expireAfterWrite(es, java.util.concurrent.TimeUnit.SECONDS)
                     .build()
                     .asMap();"
                       `((es ,(->jlong expiration-seconds))))))
      (make-parameter-with-storage* initial-value storage)))

  ;; Parameter-like construction that receives either a value or a thunk.
  ;; When called returns the value of executing the thunk or the provided value.
  ;; Good for mixing parameters & futures, among other things.
  (define* (make-thunk-unpacking-parameter* (init #f))
    (let ((inner-parameter (make-parameter* (if (procedure? init) init (lambda () init)))))
      (case-lambda    (() (and-let* ((inner-result (inner-parameter))) (inner-result)))
                 ((new) (let ((thunk (if (procedure? new) new (lambda () new))))
                          (and-let* ((inner-result (inner-parameter thunk))) (inner-result)))))))

  ;; A parameter that will block callers until a value is available, if unset.
  (define* (make-blocking-parameter* (init #f))
    (let ((inner-parameter (make-parameter* init))
          (inner-semaphore (make-semaphore)))
      (case-lambda    (() (let loop () (let ((result (inner-parameter)))
                                    (if result result
                                        (begin
                                          (inner-semaphore 'acquire)
                                          (loop))))))
                 ((new) (let ((previous-data (inner-parameter new)))
                          (when new (inner-semaphore 'release-all-blocked))
                          previous-data)))))


    ; Generates parameters that work and are safe across threads.
  (define* (make-parameter-with-storage* (throw-error-upon-void: throw-error-upon-void #f) init storage)
    (define (false-if-java-null o) (if (java-null? o) #f (java-unwrap o)))
    
    (j "tmap .put(\"SINGLEKEY\", value);" `((value ,(java-wrap init)) (tmap ,storage)))
    ((lambda ()
       (define calculate-result
         (lambda (parameter-provided value)
           (if (not parameter-provided)
               (let ((result (false-if-java-null (j "tmap.get(\"SINGLEKEY\");" `((tmap ,storage))))))
                 result)
               (let ((result (calculate-result #f 'ignored)))
                 (j "tmap.put(\"SINGLEKEY\", value);" `((value ,(java-wrap value)) (tmap ,storage)))
                 result))))
       (case-lambda
         (()
          (let ((result (calculate-result #f 'ignored)))
            (when (and throw-error-upon-void (void? result))
              (throw (make-error "Value requested for unset (make-parameter*) object. No-args constructor disallows (void) results.")))
            result))
         ((init)
          (calculate-result #t init))))))

  
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
    (d/n (string-append*
          "Sample usage - 1: " (iasylum-write-string
                            `(subtract-dates (date->time-utc (string->date "2016-03-05T10:21:00Z" "~Y-~m-~dT~k:~M:~S~z"))
                                             "2016-04-05T15:15:00Z"))
          "\n"
          "Sample usage - 2: " (iasylum-write-string
                                `(inexact->exact (ceiling (/ (time-second (subtract-dates (current-date-utc)
                                                                                          "2016-04-05T15:15:00Z")) 60.0)))))))
   (( (? string? later) (? string? earlier))
    (time-difference (date->time-utc (string->date later "~Y-~m-~dT~k:~M:~S~z"))  (date->time-utc (string->date earlier "~Y-~m-~dT~k:~M:~S~z"))))
   (( (? date? later) (? date? earlier))
    (time-difference (date->time-utc later)  (date->time-utc earlier)))
   (( (? string? later) (? date? earlier))
    (time-difference (date->time-utc (string->date later "~Y-~m-~dT~k:~M:~S~z"))  (date->time-utc earlier)))
   (( (? date? later) (? string? earlier))
    (time-difference (date->time-utc later)  (date->time-utc (string->date earlier "~Y-~m-~dT~k:~M:~S~z"))))))
  
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
     (anything (error "Invalid parameter to add-between " anything))))

  (define (add-between-list what l)
    (assert (list? l))
    (letrec ((workhorse  (match-lambda
                      (() ())
                      ((single-element) (list single-element))
                      ((first-element . rest) (cons first-element (cons what (workhorse rest))))
                      (anything (throw (make-error (string-append* "Invalid parameter" anything)))))))
      (workhorse l)))

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

  (define nan? (lambda (x) (not (or (< x 777) (> x -777)))))
  
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
    (let ((r (exact->inexact number)))
      (or (and (not (nan? r)) r)
          (string->number (->string (number->jbigdecimal number))))))

  ;;
  ;; (define a +)
  ;; (apply a '(1 1)) => 2
  ;; (apply* a '(1 1)) => 2
  ;; (apply* "a" '(1 1)) => 2
  ;; (apply* 'a '(1 1)) => 2
  ;;
  (define (apply* fn-name param-list)
    (if (procedure? fn-name)
        (apply fn-name param-list)
        (let ((fn (getprop (string->symbol (string-append* fn-name)) (interaction-environment))))
          (if (not fn)
              (throw (make-error (format "Function \"~a\" not found!" fn-name)))
              (apply fn param-list)))))

  ;; This appends the provided lists ignoring any possible #f arguments.
  (define (append-lists-when . p)
    (apply
     append
     (map (lambda (p)
            (let ((is-list (list? p)))
              (if is-list
                  p
                  (begin
                    (assert (boolean? p) (not p)
                            "WARNING/EXPLANATION OF WHY THIS FAILED"
                            "append-lists-when assumes all arguments are either lists or #f which is interpreted as an empty list."
                            "a non-#f value which is not a list is an invalid argument")
                    '()))))
          p)))

  ;;
  ;; A let-like utility that runs the code to define each binding in parallel.
  ;;
  ;; Example:
  ;; (time (let-parallel [(a (begin (sleep 1000) (+ 1 1))) (b (begin (sleep 500) (+ 2 2)))] (+ a b)))
  ;; => (6 (1001 ms))
  ;;
  ;; (time (let [(a (begin (sleep 1000) (+ 1 1))) (b (begin (sleep 500) (+ 2 2)))] (+ a b)))
  ;; => (6 (1500 ms))
  ;;
  (define-syntax let-parallel
    (syntax-rules ()
      ((_ [(var-name value) ...] body ...)
       (call-with-values
           (lambda ()
             (watched-parallel (lambda ()
                         value) ...))
           (lambda (var-name ...)
             body ...)))))

  ;;
  ;; A version of map that runs the code in parallel.
  ;;
  ;; Example:
  ;; (time (map-parallel (lambda (a)
  ;;          (begin
  ;;            (sleep 1000)
  ;;            (+ 1 a)))
  ;;       '(1 2 3 4 5)))
  ;; => ((2 3 4 5 6) (1000 ms))
  ;;
  (define map-parallel (match-lambda* [((? procedure? fn)
                                        (element ___))
                                       (multiple-values->list
                                        (apply watched-parallel (map (lambda (i)
                                                               (delay (fn i)))
                                                             element)))]))

  ;;
  ;; Use like this:
  ;;
  ;; (atomic-exection "lock name" body ...)
  ;;
  (define-syntax atomic-execution
    (syntax-rules ()
      ((_ lock-name body ...)
       (dynamic-wind (lambda ()
                       (mutex/lock! (mutex-of lock-name)))
                     (lambda ()
                       body ...)
                     (lambda ()
                       (mutex/unlock! (mutex-of lock-name)))))))

  ;;
  ;; (select-sublist '(a b c d e) 1 3) => (b c d)
  ;; (select-sublist '(a b c d e) 1 10) => (b c d e)
  ;; (select-sublist '(a b c d e) 10 1) => ()
  ;;
  (define (select-sublist lst initial-index end-index)
    (or (and (or (> initial-index end-index)
                 (null? lst))
             '())
        (let* ((size (length lst))
               (last-index (- size 1))
               (end-index (max 0 (min end-index last-index)))
               (initial-index (min (max 0 initial-index) size)))
          (drop-right (drop lst initial-index)
                      (- last-index end-index)))))

  ;; Shuffles a list to a random order.
  (define (shuffle x)
    (do ((v (list->vector x)) (n (length x) (- n 1)))
        ((zero? n) (vector->list v))
      (let* ((r (random n)) (t (vector-ref v r)))
        (vector-set! v r (vector-ref v (- n 1)))
        (vector-set! v (- n 1) t))))

  (define* (make-future l)
    (let* ((define-future-result (make-parameter* #f))
           (thread-handle (watched-thread/spawn (lambda () (let ((result (try-and-if-it-fails-or-empty-or-java-null-return-object () (l)))) (define-future-result result))))))
      (lambda* ((timeout-milliseconds: timeout #f) (timed-out-value: timed-out-value #f))
        (if timeout
            (if (thread/join thread-handle timeout) (define-future-result) timed-out-value)
            (and (thread/join thread-handle) (define-future-result))))))

  ;;
  ;; "only" is like "first" but ensure that the list has one element
  ;; and rises an error otherwise.
  ;;
  (define (only lst)
    (assert (and (= (length lst) 1)
                 (first lst))))

  ;;
  ;; (sum-alist '(("a" . 5) ("a" . 10) ("b" . 23) ("b" . 20) ("a" . 30)))
  ;; => (("a" . 45) ("b" . 43))
  ;;
  (define* (sum-alist alist (sum-fn: sum-fn +))
    (fold (match-lambda* (((key . value) acc)
                          (or (and-let* ((v (get key acc))
                                         (new-acc (alist-delete key acc))
                                         (new-sum (sum-fn v value))
                                         (result (cons `(,key . ,new-sum) new-acc)))
                                        result)
                              (cons `(,key . ,value) acc))))
          (list) alist))

  (define (base64-encode o)
    (->scm-object
     (j "javax.xml.bind.DatatypeConverter.printBase64Binary(data.toString().getBytes());"
        `((data ,(->jobject o))))))

  (define (base64-decode s)
    (->scm-object
     (j "new String(javax.xml.bind.DatatypeConverter.parseBase64Binary(data.toString()));"
        `((data ,(->jobject s))))))

  ;;
  ;; Apply the binary-fn to each element on the list from left to right,
  ;; grouping by the key of the alist. The original order of elements is not kept.
  ;;
  ;; Example:
  ;;
  ;; (group-by-key-and-apply + 0 '(("a" . 3) ("b" . 4) ("d" . 8) ("b" . 6)))
  ;; => (("b" . 10) ("d" . 8) ("a" . 3))
  ;;
  ;; (group-by-key-and-apply * 1 '(("a" . 3) ("b" . 4) ("d" . 8) ("b" . 6)))
  ;; => (("b" . 24) ("d" . 8) ("a" . 3))
  ;;
  ;; (group-by-key-and-apply append '() '(("a" . (1 2 3)) ("b" . (4 5 6)) ("d" . (7 8 9)) ("b" . (10 11 12))))
  ;; => (("b" 4 5 6 10 11 12) ("d" 7 8 9) ("a" 1 2 3))
  ;;
  (define (group-by-key-and-apply binary-fn neutral-value alist)
    (fold (lambda (current acc)
            (let* ((key (car current))
                   (value (cdr current))
                   (total (get key acc neutral-value)))
              (cons (cons key (binary-fn total value))
                    (remove (lambda (e) (equal? key (car e))) acc))))
          '()
          alist))

  ;;
  ;; (validate-cpf "18767017495") => #t
  ;; (validate-cpf "170.010.386-52") => #t
  ;; (validate-cpf "170.010.386-53") => #f
  ;;
  (define (validate-cpf cpf)
    (let ((cpf (string-filter (string->char-set "0123456789") (string-trim-both cpf))))
      (and (not (string=? cpf "00000000000"))
           (not (string=? cpf "11111111111"))
           (not (string=? cpf "22222222222"))
           (not (string=? cpf "33333333333"))
           (not (string=? cpf "44444444444"))
           (not (string=? cpf "55555555555"))
           (not (string=? cpf "66666666666"))
           (not (string=? cpf "77777777777"))
           (not (string=? cpf "88888888888"))
           (not (string=? cpf "99999999999"))
           (let ((validate (lambda (base)
                             (let* ((rest (mod (* 10 (reduce + 0 (list-ec (: i 1 (+ base 10))
                                                                          (* (string->number (substring/shared cpf (- i 1) i))
                                                                             (- (+ base 11) i)))))
                                               11))
                                    (rest (if (or (= rest 10) (= rest 11))
                                              0 rest)))
                               (= rest (string->number (substring/shared cpf (+ base 9) (+ base 10))))))))
             (and (validate 0)
                  (validate 1))))))

  ;;
  ;; Get environment variable.
  ;;
  ;; (get-env "PATH") => /usr/local/sbin:/usr/local/bin
  ;;
  (define (get-env name)
    (let ((environment-variable-value (j "System.getenv(definedname);" `((definedname ,(->jstring name))))))
      (and (not (java-null? environment-variable-value)) (->string environment-variable-value))))

  ;;
  ;; It can receive a time, date or jdate.
  ;; E.g.: 5 minutes ago.
  ;;
  (define (get-relative-time date-or-time)
    (->string (j "new org.ocpsoft.prettytime.PrettyTime().format(dot);"
                 `((dot ,(->jobject date-or-time))))))

  ;;
  ;; It can receive a time, date or jdate.
  ;; E.g.: 5 minutes.
  ;;
  (define (get-relative-duration start-date-or-time end-date-or-time)
    (->string (j "new org.ocpsoft.prettytime.PrettyTime(obj1).format(obj2);"
                 `((obj1 ,(->jobject start-date-or-time))
                   (obj2 ,(->jobject end-date-or-time))))))

  (define (pretty-print-to-string obj)
    (let ((stro (open-output-string)))
      (pretty-print obj stro)
      (get-output-string stro)))

  (define (split-string-comma str)
    (let ((split-regex (irregex '(+ ","))))
      (irregex-split split-regex str)))

  (define (split-string token str)
    (let ((split-regex (irregex `(+ ,token))))
      (irregex-split split-regex str)))

  (define (format-message msg params)
    (->scm-object
     (j "java.text.MessageFormat.format(msg, params);"
        `((msg ,(->jstring msg))
          (params ,(jlist->jarray (->jobject (map (lambda (param)
                                                    (->jobject param))
                                                  params))))))))

  (define (clear-string str)
    (irregex-replace/all "\\W" str ""))

  (define* (adjust-date-timezone dt (timezone: timezone "Brazil/East"))
    (time-utc->date (date->time-utc dt)
                    (->number
                     (j "java.util.TimeZone.getTimeZone(timezone).getRawOffset() / 1000;"
                        `((timezone ,(->jstring timezone)))))))

  (define (simulate-error chance-porc)
    (if (< (random 100) chance-porc)
        (throw (make-error "Simulating an error!"))))

  (define (zip-with-password password source-file-path destination-file-path)
    (j "
    net.lingala.zip4j.model.ZipParameters zipParameters = new net.lingala.zip4j.model.ZipParameters();
    zipParameters.setCompressionMethod(net.lingala.zip4j.util.Zip4jConstants.COMP_DEFLATE);
    zipParameters.setCompressionLevel(net.lingala.zip4j.util.Zip4jConstants.DEFLATE_LEVEL_ULTRA);
    zipParameters.setEncryptFiles(true);
    zipParameters.setEncryptionMethod(net.lingala.zip4j.util.Zip4jConstants.ENC_METHOD_STANDARD);
    zipParameters.setPassword(password);
    net.lingala.zip4j.core.ZipFile zipFile = new net.lingala.zip4j.core.ZipFile(destinationzipfilepath);
    zipFile.addFile(new File(filepath), zipParameters);"
       `((password ,(->jstring password))
         (destinationzipfilepath ,(->jstring destination-file-path))
         (filepath ,(->jstring source-file-path))))
    destination-file-path)

  ;;
  ;; Usage:
  ;;
  ;; (select <json> field1 -> field2 -> ...)
  ;;
  ;; json can be in string format or "alist" format, like the
  ;; result of fn (vector->list_deeply (json->scheme <json>)).
  ;;
  (define-syntax select
    (syntax-rules (->)
      ((_ alist-tree-or-json p1)
       (get p1 (if (string? alist-tree-or-json)
                   (vector->list_deeply (json->scheme alist-tree-or-json))
                   alist-tree-or-json)))
      ((_ alist-tree p1 -> p2)
       (select (select alist-tree p1) p2))
      ((_ alist-tree body ... -> p-last)
       (select (select alist-tree body ...) p-last))))

  (define (url? obj)
    (and (string? obj)
         (try-and-if-it-throws-object (#f)
           (j "new java.net.URL(theurl).toURI();"
              `((theurl ,(->jstring obj)))))
         #t))

  (define (url->tmp-file url prefix suffix tmp-dir-path)
    (let ((tmp-file (j "java.io.File.createTempFile(theprefix, thesuffix, new java.io.File(thetmpdir));"
                       `((thetmpdir ,(->jstring tmp-dir-path))
                         (theprefix ,(->jstring prefix))
                         (thesuffix ,(->jstring suffix))))))
      (j "org.apache.commons.io.FileUtils.copyURLToFile(new java.net.URL(theurl), tmpfile);"
         `((theurl ,(->jstring url))
           (tmpfile ,tmp-file)))
      (->string (j "tmpfile.getPath();" `((tmpfile ,tmp-file))))))

(define* (tmp-file (prefix: prefix "bedlam_temporary_file") (suffix: suffix ".tmp") (tmp-dir-path: tmp-dir-path #f) )
    (let ((tmp-file
           (if tmp-dir-path
               (j "java.io.File.createTempFile(theprefix+\"_\", thesuffix, new java.io.File(thetmpdir));"
                  `((thetmpdir ,(->jstring tmp-dir-path))
                    (theprefix ,(->jstring prefix))
                    (thesuffix ,(->jstring suffix))))
               (j "java.io.File.createTempFile(theprefix+\"_\", thesuffix);"
                  `((theprefix ,(->jstring prefix))
                    (thesuffix ,(->jstring suffix)))))))
      (->string (j "tmpfile.getPath();" `((tmpfile ,tmp-file))))))

  ;;
  ;; HTML encode anything: string, lists, vectors, json schemes etc.
  ;;
  ;; Use this to avoid XSS attacks.
  ;;
  (define (html-encode obj)
    (cond [(vector? obj)
           (list->vector
            (map (lambda (e)
                   (html-encode e))
                 (vector->list obj)))]
          [(pair? obj)
           (cons (html-encode (car obj))
                 (html-encode (cdr obj)))]
          [(string? obj)
           (->scm-object (j "org.owasp.encoder.Encode.forHtml(a);" `((a ,(->jstring obj)))))]
          [else obj]))

  ;; Orderly shutdown.
  (define* (shutdown (return-code 0))
    (define-java-class |java.lang.System|)
    (define-generic-java-method exit)

    (exit (java-null |java.lang.System|) (->jint return-code)))

  ;; Immediate shutdown by any means necessary.
  (define (panic)
    (define-java-class |java.lang.System|)
    (define-generic-java-method exit)
    (watched-thread/spawn
     (lambda ()
       (exit (java-null |java.lang.System|) (->jint -1))))
    (watched-thread/spawn
     (lambda ()
       (r/s (string-append*  "kill -9 " (->scm-object (j "java.lang.ProcessHandle.current().pid();"))))))
    (exit (java-null |java.lang.System|) (->jint -1)))

  ;;
  ;; Thread safe counter.
  ;;
  ;; (define a (create-counter))
  ;; (a) => 0
  ;; (a) => 1 ...
  ;;
  ;; (define a (create-counter 1))
  ;; (a) => 1
  ;; (a) => 2 ...
  ;;
  (define* (create-counter (initial-value 0))
    (let ((c (make-atomic-long initial-value)))
      (lambda ()
        (get-and-inc-atomic-long! c))))

  ;;
  ;; See https://clojuredocs.org/clojure.core/-%3E
  ;; for more info.
  ;;
  ;; (string-append (string-titlecase "abracada bra") "a" "x")
  ;; (-> "abracada bra" string-titlecase (string-append "a" "x"))
  ;;
  ;; (string-reverse (string-titlecase "xpto"))
  ;; (-> "xpto" string-titlecase string-reverse)
  ;;
  ;; (string-titlecase (string-reverse (string-titlecase "xpto")))
  ;; (-> "xpto" string-titlecase string-reverse string-titlecase)
  ;;
  (define-syntax ->
    (syntax-rules ()
      ((_ a) a)
      ((_ a (b c ...)) (b a c ...))
      ((_ a b) (b a))
      ((_ a b c) (-> (-> a b) c))
      ((_ a b ... c) (-> (-> a b ...) c))))

  ;;
  ;; See https://clojuredocs.org/clojure.core/-%3E%3E
  ;; for more info.
  ;;
  ;; (reduce + 0 (take '(1 2 3 4 5) 2))
  ;; (->> 2 (take '(1 2 3 4 5)) (reduce + 0))
  ;;
  ;; (reduce / 1 (take '(4 3 2 1) 3))
  ;; (->> 3 (take '(4 3 2 1)) (reduce / 1))
  ;;
  (define-syntax ->>
    (syntax-rules ()
      ((_ a) a)
      ((_ a (b c ...)) (b c ... a))
      ((_ a b) (b a))
      ((_ a b c) (->> (->> a b) c))
      ((_ a b ... c) (->> (->> a b ...) c))))

  (define* (ellipsis... str (max-length 64))
    (cond [(<= (string-length str) max-length)
           str]
          [(< max-length 4) (string-take str max-length)]
          [else (format "~a..." (string-take str (- max-length 3)))]))

  ;;
  ;; Get the week number according to ISO 8601 (see https://en.wikipedia.org/wiki/ISO_week_date )
  ;; The SRFI 19 doesn't work because a fix was made AFTER this SISC
  ;; bedlam version (see https://srfi.schemers.org/srfi-19/srfi-19.html for
  ;; more info).
  ;;
  ;; See https://stackoverflow.com/a/34987052/450148 for a better explanation of this
  ;; solution in Java.
  ;;
  ;; If delta-week is set then we add (subtract if negative) the number of weeks in the result.
  ;;
  (define* (week-number-iso8601 (delta-week 0))
    (->number (j "java.time.LocalDate.now().plusWeeks(weeks).get(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR);"
                 `((weeks ,(->jlong delta-week))))))

  ;;
  ;; Get the last week of an year.
  ;;
  ;; From: https://stackoverflow.com/a/59936177/450148
  ;;
  (define (get-last-week-iso8601 year)
    (->number (j "(java.time.LocalDate.of(year, 1, 1).getDayOfWeek() == java.time.DayOfWeek.THURSDAY ||
        java.time.LocalDate.of(year, 12, 31).getDayOfWeek() == java.time.DayOfWeek.THURSDAY) ? 53 : 52"
                 `((year ,(->jint year))))))

  ;;
  ;; Returns the current year week based. If you call this fn
  ;; in 30 Dec 2019 e.g. the result is 2020. For more info, see
  ;; java.time.temporal.IsoFields.WEEK_BASED_YEAR documentation.
  ;;
  ;; If delta-week is set then we add (subtract if negative) the number of weeks in the result.
  ;;
  (define* (week-based-year-iso8601 (delta-week 0))
    (->number (j "java.time.LocalDate.now().plusWeeks(weeks).get(java.time.temporal.IsoFields.WEEK_BASED_YEAR);"
                 `((weeks ,(->jlong delta-week))))))

  (define* (week-iso8601-string-representation (week-number #f) (year #f))
    (format "~a-W~a"
            (or year (week-based-year-iso8601))
            (irregex-replace/all
             " "
             (format "~2F"
                     (or week-number (week-number-iso8601))) "0")))

  ;;
  ;; Returns a pair with the interval of the dates in the specified week,
  ;; or of the current week if not specified.
  ;;
  ;; If you want to get the previous week interval, e.g., you make:
  ;;
  ;; (get-date-interval-of-week
  ;;       (week-number-iso8601 -1)
  ;;       (week-based-year-iso8601 -1))
  ;;
  (define* (get-date-interval-of-week (week-number #f) (year #f))
    (let* ((week-number (or week-number (week-number-iso8601)))
           (year-local-date (j "java.time.LocalDate.now();"))
           (year-local-date (if year
                                (j "yld.with(java.time.temporal.IsoFields.WEEK_BASED_YEAR, year);"
                                   `((year ,(->jint year))
                                     (yld ,year-local-date)))
                                year-local-date)))
      (cons (jdate->date (j "java.util.Date.from(yld
                               .with(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR, week)
                               .with(java.time.temporal.TemporalAdjusters.previousOrSame(java.time.DayOfWeek.MONDAY))
                               .atStartOfDay(java.time.ZoneId.systemDefault()).toInstant());"
                            `((week ,(->jint week-number))
                              (yld ,year-local-date))))
            (jdate->date (j "java.util.Date.from(yld
                               .with(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR, week)
                               .with(java.time.temporal.TemporalAdjusters.nextOrSame(java.time.DayOfWeek.MONDAY))
                               .atStartOfDay(java.time.ZoneId.systemDefault()).toInstant());"
                            `((week ,(->jint week-number))
                              (yld ,year-local-date)))))))

   (define (eval-string str)
     (with-input-from-string str
       (lambda ()
         (let loop ((previous-result (void))
                    (form (read)))
        (if (eof-object? form)
            previous-result
            (loop (eval form) (read)))))))

   ;; Evals and normally returns a string with whatever was
   ;; displayed and also the result of the last form.
   ;; Exception: if nothing got displayed and the last form
   ;; returns void, #f will be returned.
   (define (eval/string str)
     (let* ((had-non-void-result (make-parameter* #f))
            (result-string
             (with-output-to-string
               (lambda ()
                 (let ((result (eval-string str)))
                   (unless (void? result)
                     (begin
                       (had-non-void-result #t)
                       (display result))))))))
       (if (not (string=? ""result-string))
           result-string
           (if (had-non-void-result) result-string #f))))

 ;;
 ;; The global shared var `amb-fail` is called to backtrack when a
 ;; condition fails.  at the top level, however, there is no
 ;; more to backtrack, so we signal an error with srfi 23.
 ;;
 (define amb-fail
   (lambda ()
     (error "amb tree exhausted")))

 ;;
 ;; ** BE CAREFUL: IT ISN'T THREAD SAFE! USE `amb-safe-context` to make it thread-safe. **
 ;;
 ;; Amb was first proposed by John McCarthy, the inventor of LISP, in his 1963 paper
 ;; A Basis for a Mathematical Theory of Computation (More of McCarthy's work can be found
 ;; on his homepage). There is also a considerably sized section in SICP on the topic
 ;; of amb & non-deterministic computation.
 ;;
 ;; Implementation from: http://community.schemewiki.org/?amb
 ;;                      Arc: http://archive.is/6rq6k
 ;;
 ;; More implementations and info at:
 ;; - https://www.shido.info/lisp/scheme_amb_e.html
 ;;   Arc: http://archive.vn/hUIdX
 ;; - https://ds26gte.github.io/tyscheme/index-Z-H-16.html#node_chap_14
 ;;   Arc: http://archive.vn/T3YLD
 ;;
 ;; Example of usage:
 ;; (let ((a (amb 1 2 3 4)))
 ;;       (if (not (= a 3))
 ;;           (amb)
 ;;         a))
 ;; => 3
 ;;
 ;; See also `amb-safe-context` macro.
 ;;
 (define-syntax amb
   (syntax-rules ()
     ((amb) (amb-fail))                  ; two shortcuts.
     ((amb expression) expression)
     ((amb expression ...)
      (let ((fail-save amb-fail))
        ((call-with-current-continuation ; capture a continuation to
           (lambda (k-success)           ;   which we return possibles.
             (call-with-current-continuation
               (lambda (k-failure)       ; k-failure will try the next
                 (set! amb-fail          ;   possible expression.
                       (lambda () (k-failure #f)))
                 (k-success              ; note that the expression is
                  (lambda ()             ;   evaluated in tail position
                    expression))))       ;   with respect to amb.
             ...
             (set! amb-fail fail-save)   ; finally, if this is reached,
             fail-save)))))))            ;   we restore the saved fail.

 (define (amb-require condition)
   (if (not condition)
       (amb-fail)))

 ;;
 ;; As an auxiliary example, `amb-possibility-list` is a special form
 ;; that returns a list of all values its input expression may return.
 ;;
 ;; it is the same as `bag-of` o Teach Yourself Scheme in Fixnum Days cap 14.
 ;;
 (define-syntax amb-possibility-list
   (syntax-rules ()
     ((amb-possibility-list expression)
      (let ((value-list '()))
        ;; this requires that amb try its sub-forms left-to-right.
        (amb (let ((value expression))
               (set! value-list (cons value value-list))
               (amb-fail))
             (reverse value-list))))))   ; order it nicely.

 ;;
 ;; `amb-safe-context` is an auxiliary fn
 ;; to make `amb` usage thread-safe. Also,
 ;; any exception/error will call (amb) as default,
 ;; so instead have to use (amb) or (amb-fail) to
 ;; sign a failure you can just throw an error.
 ;;
 ;; If you use threads, in the same JVM you must
 ;; call `amb` always inside `amb-safe-context`.
 ;; Any single call alone can ruin all others "safe contexts"
 ;; because the global shared var `amb-fail` will be dirty.
 ;;
 ;; Notice that the entire block inside `amb-safe-context`
 ;; will execute atomically / synchronized.
 ;;
 ;; Example of usage:
 ;;
 ;; (amb-safe-context
 ;;   (amb (error "error")
 ;;     (let ((a (amb 1 2 3 4)))
 ;;       (if (not (= a 3))
 ;;           (error "wrong number")
 ;;         a))))
 ;;
 ;; => 3
 ;;
 ;; See also `amb`.
 ;;
 (define-syntax amb-safe-context
   (syntax-rules ()
     [(_ body ...)
      (mutex/synchronize
       (mutex-of amb-fail)
       (lambda ()
         (with-failure-continuation
          (lambda (err context)
            (amb))
          (lambda ()
            body ...))))]))

  (create-shortcuts (avg -> average))

  (define-generic-java-method release)
  (define-generic-java-method acquire)
  (define-generic-java-method available-permits)
  (define-generic-java-method has-queued-threads)
  
  (reset-d)
  (reset-w)  
)
