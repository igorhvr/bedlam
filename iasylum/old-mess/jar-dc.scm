; DONE Uncompresses a .jar to a directory.
; DONE Finds each .class file there.
; DONE Calls a decompiler on each one - dumping the java output to some suitable place.
; TODO Generate a build.xml file able to call a java compiler on all source files found
; DONE Packs this in the output jar.



(define (r cmdline)
	(run/string (sh ,"-c" ,cmdline)))

(define (rt cmdline)
	(run (sh ,"-c" ,cmdline)))

(define (unzip fn dest-path)
  (r (string-append "yes | unzip " fn " -d " dest-path)))

(define (zip-directory directory zipfn)
  (r (string-append "pushd . ; cd " directory " ; yes | zip -9 -r " zipfn " . ; popd " )))

(define (rglob d)
  (define (do-rglob d)
    (let ( (d-contents (glob d)) )
      (map (lambda e
	     (let ((se (car e)))
	       (if (file-directory? se) (do-rglob (string-append se "/*")) se)))
	   d-contents)))
  (flatten (do-rglob (string-append d "/*"))))

(define (flatten l)
  (cond ((null? l) '())
        ((list? l)
         (append (flatten (car l)) (flatten (cdr l))))
        (else (list l))
  )
)

(define (generate-unique-name prefix)
  (let ((result (string-append prefix "_" (number->string (generate-unique-number)))))
    (if (file-exists? result) (generate-unique-name prefix)
	result)))

(define (create-temp-folder)
  (let ((dir-name (generate-unique-name "/tmp/jar-dc-tmp_")))
    (rt (string-append "mkdir -p " dir-name))
    dir-name))

(define generate-unique-number
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

; (create-temp-folder)
  
; jad -s java -d no -r `find | grep .class`

; Single-file decompile.
(define (java-decompile class-file-name)
  (r (string-append "jad -p " class-file-name)))

(define (java-classfile-filter p)
  (and 
      (regexp-search? (uncase (rx ".class" eos)) p)
      (not (regexp-search? (rx "$") p))))

(define (java-generate-source-jar fn)
  (display "\n\nGenerating source jar for " ) (display fn) (display "\n\n")
  (if (regexp-search? (uncase (rx ".jar" eos)) fn)
      (let* ((src-file-name (string-append (substring fn 0 (- (string-length fn) 4)) "-src.jar"))
	     (tmp-folder (create-temp-folder))
	     (output-folder (create-temp-folder))
	     (ignored (unzip fn tmp-folder))
	     (class-files (filter java-classfile-filter (rglob tmp-folder) ))
	     (process-element (lambda (e) (r (string-append "jad -s java -d " output-folder " -r " e)))))
	(for-each process-element class-files)
	(zip-directory output-folder src-file-name)
	(display "\n\n\n\n\n\n\n\n"))
      "error - extension must be .jar"))
    
; jad -s java -d no -r `find | grep .class`


;(java-generate-source-jar "/tmp/startup.jar")

(define do-all
  (lambda ()
    (java-generate-source-jar "/tmp/j1.jar")
    (java-generate-source-jar "/tmp/j2.jar")))

(do-all)
    