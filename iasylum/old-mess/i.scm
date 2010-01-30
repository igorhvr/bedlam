; (load "/Igor/i/i.scm")
(define d (lambda p (for-each display p)))

(define counter 2000) ; Don't redefine this unless needed!


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

;;; Start - RANDOM UTILITIES ;;;
; A call to the random-maker procedure presented here yields a
; dynamically constructed procedure that acts as a random-number
; generator. When the dynamically constructed procedure is invoked
; with no arguments, it returns a pseudo-random real value evenly
; distributed in the range [0.0, 1.0); when it is invoked with
; one argument (which should be a positive integer n), it returns a
; pseudo-random integer value evenly distributed in the range [0, n);
; when it is invoked with two arguments, the first of which should be a
; positive integer and the second the symbol reset, it changes the seed
; of the random-number generator to the value of the first argument.
; 
; The generator employs the linear-congruential method, and
; specifically uses a choice of multiplier that was proposed as a
; standard by Stephen K. Park et al. in ``Technical correspondence,''
; Communications of the ACM 36 (1993), number 7, 108--110.
; 
; 

(define generate-error-message (lambda () (/ 1 0)))

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647) (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
;                19860617))))  ;; an arbitrarily chosen birthday
    generate-error-message))))  ;; an arbitrarily chosen birthday 
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed)) (/ (- modulus
               seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1 exact-top)))
                 (set! seed (apply-congruence seed)) (inexact->exact
                 (+ 0.0 (floor (* top (/ seed modulus)))) )))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))

(define sample-random
  (random-maker 19781116))  ;; another arbitrarily chosen birthday
; 
; The random procedure added at the end shows how to call
; random-maker to get a random-number generator with a specific
; seed. The random-number generator itself is invoked as described
; above, by such calls as (random), to get a real number between 0
; and 1, and (random n), to get an integer in the range from 0 to
; n - 1.  The location of the binding of seed -- inside the body
; of random-maker, but outside the lambda-expression that denotes
; the dynamically allocated procedure -- ensures that the storage
; location containing the seed will be different for each invocation of
; random-maker (so that every generator that is constructed will have
; an independently settable seed), yet inaccessible except through
; invocations to the dynamically allocated procedure itself. In
; effect, random-number generators in this implementation constitute
; an abstract data type with the constructor random-maker and exactly
; three operations, corresponding to the three possible arities of
; a call to the generator.
; 
; When calling this procedure, the programmer must supply an initial
; value for the seed. This should be an integer (if it is not, an
; arbitrary default seed is silently substituted). The value supplied
; is forced into the range (0, modulus], since it is an invariant of
; the procedure that the seed must always be in this range.
; 
; To obtain an initial seed that is likely to be different each time a
; new generator is constructed, use some combination of the program's
; running time and the wall-clock time. (Most Scheme implementations
; provide procedures that return one or both of these quantities. For
; instance, in SCM, the call
; 
; 
; (random-maker (+ (* 100000 (get-internal-run-time)) (current-time)))
; 
; yields a generator with an effectively random seed.)
; 
; 
; --------------------------------------------------------------------------------
; 
; This document is available on the World Wide Web as
; 
; 
; http://www.math.grin.edu/~stone/events/scheme-workshop/random.html
; 
; 
; --------------------------------------------------------------------------------
; created July 10, 1995 last revised June 21, 1996
; 
; John David Stone (stone@math.grin.edu)

(define random
  (let ((number-from-time (call-with-values  (lambda () (time+ticks)) (lambda (time ticks) (+ (* 1000000 time) ticks)))))
    (let ((random-function (random-maker number-from-time)))
      (lambda ()
        (random-function 1000000000000)))))


  
;;; End  - RANDOM UTILITIES ;;;

; Multiple-parameter-accepting display.
;(define-syntax d
;  (syntax-rules ()
;    ((d arg) (display arg))
;    ((d arg1 arg2 ...) (begin (display arg1) (d arg2 ...)))
;    ))

(define-syntax when
  (syntax-rules ()
    ( (_ c p ...) (if c (begin p ... )))))

(define-syntax unless
  (syntax-rules ()
    ((when cond exp ...)
     (if (not cond) (begin exp ...)))))

(define memoize-cache '())
(define show-cache
  (lambda ()
    (display memoize-cache)))
(define clear-cache
  (lambda ()
    (define clear-item
      (lambda (i)
        (list)))
    (set! memoize-cache (map clear-item memoize-cache))))

(define memoized-warnings-on #t)

(define memoize-enabled #t)

(define (memoize func)
  (let ((mypos (length memoize-cache)))
    (if (not memoize-enabled)
        (apply func args)
        (begin
          (set! memoize-cache (reverse (cons (list) (reverse memoize-cache))))
          (lambda args
            (let ((memoresults (list-ref memoize-cache mypos)))
              (let ((memoized-pair (assoc args memoresults)))
                                        ; for debugging:
                (when (and memoized-warnings-on memoized-pair) (d "\nWarning: Call to " func " returning from cache.\n"))
                (if memoized-pair (cdr memoized-pair)
                    (let ((thing-to-save (apply func args)))
                      (begin
                        (set! memoresults (cons (cons args thing-to-save) memoresults))
                        (let ((the-v (list->vector memoize-cache)))
                          (vector-set! the-v mypos memoresults)
                          (set! memoize-cache (vector->list the-v)))
                        thing-to-save))))))))))

(define (original-memoize func)
  (let ((memoresults (list)))
    (lambda args
      (let ((memoized-pair (assoc args memoresults)))
	; for debugging:
	; (if memoized-pair (begin (print memoized-pair) (cdr memoized-pair))
	(if memoized-pair (cdr memoized-pair)
	  (let ((thing-to-save (apply func args)))
	    (begin
	      (set! memoresults (cons (cons args thing-to-save) memoresults))              
	      thing-to-save
	    )
	  )
        )
      )
    )
  )
)

(define (r cmdline)
	(run/string (sh ,"-c" ,cmdline)))

(define (rt cmdline)
	(run (sh ,"-c" ,cmdline)))

; ---

; FILE		"substr.scm"
; IMPLEMENTS	Substring search
; AUTHOR	Ken Dickey
; DATE		1991 August 6
; LAST UPDATED

; NOTES	
;	Based on "A Very Fast Substring Search Algorithm", Daniel M. Sunday,
;	CACM v33, #8, August 1990.
;

;; Gambit-specific compile options

;(##declare
;  (ieee-scheme)
;  (standard-bindings)
;  (lambda-lift)
;  (block)
;  (fixnum))

;;
;; SUBSTRING-SEARCH-MAKER takes a string (the "pattern") and returns a function
;; which takes a string (the "target") and either returns #f or the index in
;; the target in which the pattern first occurs as a substring.
;;
;; E.g.: ((substring-search-maker "test") "This is a test string")  -> 10
;;       ((substring-search-maker "test") "This is a text string")  -> #f

(define (SUBSTRING-SEARCH-MAKER pattern-string)

  (define NUM-CHARS-IN-CHARSET 256)  ;; Update this, e.g. for ISO Latin 1


  (define (BUILD-SHIFT-VECTOR pattern-string)
    (let* ( (pat-len (string-length pattern-string))
  	    (shift-vec (make-vector NUM-CHARS-IN-CHARSET (+ pat-len 1)))
	    (max-pat-index (- pat-len 1))
          )
      (let loop ( (index 0) )
        (vector-set! shift-vec 
		     (char->integer (string-ref pattern-string index))
		     (- pat-len index)
        )
	(if (< index max-pat-index)
	    (loop (+ index 1))
	    shift-vec)
  ) ) )


  (let ( (shift-vec (BUILD-SHIFT-VECTOR pattern-string))
	 (pat-len   (string-length pattern-string))
       )

   (lambda (target-string)

      (let* ( (tar-len (string-length target-string))
	      (max-tar-index (- tar-len 1))
	      (max-pat-index (- pat-len 1))
            )
	(let outer ( (start-index 0) )
           (if (> (+ pat-len start-index) tar-len)
	       #f
	       (let inner ( (p-ind 0) (t-ind start-index) )
	          (cond
	  	   ((> p-ind max-pat-index)  ; nothing left to check
		    #f  		     ; fail
		   )
		   ((char=? (string-ref pattern-string p-ind)
		 	    (string-ref target-string  t-ind))
		    (if (= p-ind max-pat-index)
		        start-index  ;; success -- return start index of match
		        (inner (+ p-ind 1) (+ t-ind 1)) ; keep checking
                    )
                   )
                   ((> (+ pat-len start-index) max-tar-index) #f) ; fail
		   (else
		     (outer (+ start-index
			       (vector-ref shift-vec
					   (char->integer 
				 		(string-ref target-string
							    (+ start-index pat-len)
                   ) )      )  )           )    )
                  ) ; end-cond
          ) ) )
   )  ) ; end-lambda
) )


;;				--- E O F ---


; ---

; string-split: splitting a string into substrings
; 
; Problem Split a string into words separated by whitespace or other
; delimiters. See the function split in Python or Perl.  Examples
;   (string-split " abc d e f  ")       ==> ("abc" "d" "e" "f")
;   (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
;   (string-split " abc d e f  " '() 0) ==> () (string-split ":"
;   '(#\:))           ==> ("" "") (string-split ":abc:d:e::f:" '(#\:))
;         ==> ("" "abc" "d" "e" "" "f" "")
;   (string-split "root:x:0:0:Lord" '(#\:) 2)
;         ==> ("root" "x:0:0:Lord")
;   (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
;         ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
;   (string-split "/usr/local/bin" '(#\/))
;         ==> ("" "usr" "local" "bin")
; Specification
;   string-split STRING              -> STRINGS string-split STRING
;   '()          -> STRINGS string-split STRING '() MAXSPLIT -> STRINGS
; These procedures return a list of whitespace-delimited words
; in STRING. Leading and trailing whitespaces of the words are
; trimmed. If STRING is empty or contains only whitespace, the empty
; list is returned.
; 
;          string-split STRING CHARSET          -> STRINGS string-split
;          STRING CHARSET MAXSPLIT -> STRINGS
; These procedures return a list of words in STRING delimited by
; the characters in CHARSET. The latter is a list of characters to
; be treated as delimiters. Leading or trailing delimiters of the
; words are not trimmed. That is, the resulting list will have as
; many initial empty string elements as there are leading delimiters
; in STRING.  If MAXSPLIT is specified and positive, the resulting
; list will contain at most MAXSPLIT elements, the last of which is
; the string remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is
; specified and non-positive, the empty list is returned. "In time
; critical applications it behooves you not to plit into more fields
; than you really need."

(define (string-split str . rest)
                ; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (+ i 1) yet-to-split-count))
        (else (scan-beg-word (+ i 1) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (+ i 1) (- yet-to-split-count 1))))
        (else (scan-word (+ 1 i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

                ; maxsplit is a positive number
                ; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (+ i 1) (- yet-to-split-count 1))))
        (else (scan-word (+ i 1) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

                        ; resolver of overloading...
                        ; if omitted, maxsplit defaults to
                        ; (+ 1 (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (+ 1 (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (+ 1 (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)


; Naive version.
;(define (flatten l)
;  (cond ((null? l) '())
;        ((list? l)
;         (append (flatten (car l)) (flatten (cdr l))))
;        (else (list l))
;  )
;)

; High performance one:
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

(define (rglob d)
  (define (do-rglob d)
    (let ( (d-contents (glob d)) )
      (map (lambda e
	     (let ((se (car e)))
	       (if (file-directory? se) (do-rglob (string-append se "/*")) se)))
	   d-contents)))
  (flatten (do-rglob (string-append d "/*"))))

(define rglob (memoize rglob))



; (define (file->string file-name)
;   (call-with-input-file file-name
;     (lambda (i)
;       (define o (make-string-output-port))
; 
;       (define (do-read i)
; 	(let ((a (read-char i)))
; 	  (if (eof-object? a) "" 
; 	      (begin (write-char a o) (do-read i))
; 	      )))
;       (do-read i)
;       (string-output-port-output o))))

(define (file->string file-name maxchars)
  (d "Doing file->string.")
  (call-with-input-file file-name
    (lambda (i)
      (let ( (result (read-string maxchars i)) )
	(if (eq? result #f) ""
	    result)))))
(define file->string (memoize file->string))

; Returns matching files for a given expression and a given list of files.
(define (matching-files e l)
  (filter (lambda (f) (not (eqv? f '())))
	  (map
	   (lambda n     
	     (let ( (contents (file->string (car n) 100000)) )
	       (if (regexp-search? (expressify e) contents)
		   (car n)
		   '())))
	   l)))
(define matching-files (memoize matching-files))

(define generate-unique-number
  (lambda ()
    (set! counter (+ counter 1))
    counter))
; Think about (glob-quote str). From the docs:
;  Returns a constant glob pattern that exactly matches str. All wild-card
;  characters in str are quoted with a backslash.
; (glob-quote "Any *.c files?") -> "Any \*.c files\?"
(define (replace-in-file file-name rx str)
  (let ( (contents (file->string file-name 1000000)) )
     (set! contents (regexp-substitute/global #f rx contents 'pre str 'post))
     ; TODO: What if we have a name clash with the generated file name?!?
     (rename-file file-name (string-append file-name ".REPLACE-IN-FILE-BACKUP-" (number->string (generate-unique-number))))
     (call-with-output-file file-name (lambda (f) (display contents f)))     
     contents))

(define numbered-keep-rx (rx ".keep."  (| ("0123456789"))));|))); Harder!

(define (gen-rx-filter rx) (lambda (p) (regexp-search? rx p)))

(define cleanup_in
  (lambda (folder)
    (define l (rglob folder))
    (define (remove-all flist)
      (if (not (eqv? '() flist))
	  (begin 
	    (delete-file (car flist)) (remove-all (cdr flist)))))
    (remove-all (filter (gen-rx-filter (rx ".keep" eos)) l))
    (remove-all (filter (gen-rx-filter (rx ".orig" eos)) l))
    (remove-all (filter (gen-rx-filter (rx ".rej" eos)) l))
    (remove-all (filter (gen-rx-filter (rx ".contrib" eos)) l))
    (remove-all (filter (gen-rx-filter (rx "REPLACE-IN-FILE-BACKUP")) l))
    (remove-all (filter (gen-rx-filter numbered-keep-rx) l))
    (remove-all (filter (gen-rx-filter (rx "~" eos)) l))
    #t))

(define (cleanup) (cleanup_in "/clear_case_storage_folder"))

(define clear_case_storage_folder->igor
  (lambda ()
    (clear_case_storage_folder->igor_in "/Igor/spsigor_data/tree")))

(define clear_case_storage_folder->igor_in
  (lambda (folder)
    (define l (rglob folder))
    (define tmf (matching-files (rx "clear_case_storage_folder") l))
    (display "\n")
    (for-each
     (lambda (f)
       (display "\nRunning replace in file: ")(display f)
       (replace-in-file f (uncase (rx "c:/clear_case_storage_folder/KDJFSLDFSA")) "c:/Igor/spsigor_data/tree")
       (replace-in-file f (uncase (rx "c:\\clear_case_storage_folder\\KDJFSLDFSA")) "c:\\Igor\\spsigor_data\\tree")
       (replace-in-file f (uncase (rx "c://clear_case_storage_folder//KDJFSLDFSA")) "c://Igor//spsigor_data//tree")
       (replace-in-file f (uncase (rx "c:\\\\clear_case_storage_folder\\\\KDJFSLDFSA")) "c:\\\\Igor\\\\spsigor_data\\\\tree"))
     tmf)
    (display "\n")
    #f))

(define igor->clear_case_storage_folder
  (lambda ()
    (define l (rglob "/Igor/spsigor_data/tree"))
    (define tmf (matching-files (rx "igor") l))
    (for-each
     (lambda (f)
       (replace-in-file f (uncase (rx "c:/Igor/spsigor_data/tree")) "c:/clear_case_storage_folder/KDJFSLDFSA")
       (replace-in-file f (uncase (rx "c:\\Igor\\spsigor_data\\tree")) "c:\\clear_case_storage_folder\\KDJFSLDFSA")
       (replace-in-file f (uncase (rx "c://Igor//spsigor_data//tree")) "c://clear_case_storage_folder//KDJFSLDFSA")
       (replace-in-file f (uncase (rx "c:\\\\Igor\\\\spsigor_data\\\\tree")) "c:\\\\clear_case_storage_folder\\\\KDJFSLDFSA"))
     tmf)))

; Junk
;(define s "clear_case_storage_folder_i")
;(regexp-substitute/global #f (rx "clear_case_storage_folder") s
;                          'pre "IGORSTORE" 'post)
; (setq scheme-program-name "/usr/local/lib/scsh/scshvm -h 90000000")
; (setq scheme-program-name "/usr/local/lib/scsh/scshvm -h 20000000")

; Program this: rm `find | grep backup$`
(define (generate-unique-name prefix)
  (let ((result (string-append prefix "_" (number->string (generate-unique-number)))))
    (if (file-exists? result) (generate-unique-name prefix)
	result)))

(define (create-temp-directory)
  (let ((dir-name (generate-unique-name "/tmp/tmp_folder_")))
    (create-directory dir-name)
    dir-name))
    
(define clear-case-get-clean-tree
  (lambda ()
    (display "\nStart - (get-clean-tree... \n")
    (let ( (tree-location (generate-unique-name "/Igor/cc_storage/ipackage") )); "\\\\lsfkladslfas-host\\tool_ccs\\ipackage")) )
      (let ( (last-pathname (string-split tree-location '(#\/))) )      
	(r (string-append "cleartool mkview -stream igorhvr_KDJFSLDFSA-P@\\\\KDJFSLDFSA -snapshot \\\\\\\\lsfkladslfas-host\\\\tool_ccs\\\\"
			  (cadddr last-pathname))))
      (r (string-append "cd " tree-location " ; cleartool update -add_loadrules .\\\\AQIEUASIQU\\\\FKDSLFSDFS >> /tmp/o.txt" ))
      (r (string-append "cd " tree-location " ; cleartool update -add_loadrules .\\\\AQIEUASIQU\\\\SDFDSFKJDSJFSDF7SDF >> /tmp/o.txt" ))
      (r (string-append "cd " tree-location " ; cleartool update -add_loadrules .\\\\KDJFSLDFSA\\\\KDSFLKDSJFLSKDJFS >> /tmp/o.txt" ))
      (display "\nEnd  - (get-clean-tree... \n")
      tree-location)))


(define harvest-get-clean-tree
  (lambda ()
    (display "\nStart - get-clean-tree \n")
    (let ( (tree-location (generate-unique-name "/tmp/hco_tmp_folder") ));
      (let ( (last-pathname (string-split tree-location '(#\/))) )
        (let ((cmd (string-append "mkdir " tree-location " ; cd " tree-location " ; hco -b " (get-harvest-server) " -en \"" (get-harvest-project-name) "\" -st " (get-harvest-state " -br -r -eh " (get-harvest-credentials-file) " -vp \\\\IGORHVR\\\\sssdsat -nvs" )))
              (display (string-append "\nWill run " cmd "\n"))
              (r cmd))
          (display "\nEnd   - get-clean-tree \n")
          tree-location)))))

(define get-clean-tree harvest-get-clean-tree)

(define generate-zips
  (lambda ()
    (define si-old (make-string-input-port (r (string-append "grep \"" "^\\-\\-\\- " "\" /tmp/m.diff"))))
    (define si-new (make-string-input-port (r (string-append "grep \"" "^+++" "\" /tmp/m.diff"))))
    
    (define (get-diff-files si)  
      (let* ((discard1 (read si))
	     (filename (read si))
	     (discard2 (read si))
	     (discard3 (read si))
	     (discard4 (read si)))
	(display "READ: ")
	(display discard1)
	(display " | ")
	(display discard2)
	(display " | ")
	(display discard3)
	(display " | ")
	(display discard4)
	(display "\n")
	(if (eof-object? discard1) '()
	    (cons 
	     (string-append (substring (symbol->string filename) 0 (string-length (symbol->string filename) )) " ")
	     (get-diff-files si)))))
    (display "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nGenerating zips from /tmp/m.diff... ")
    (let (
	  (files-old (get-diff-files si-old))
	  (files-new (get-diff-files si-new))
	  )
    
      (define cmd-old (apply string-append files-old))
      (define cmd-new (apply string-append files-new))
      
      (r "yes | rm -Rf /tmp/original_files.zip >> /tmp/o.txt")
      (r "yes | rm -Rf /tmp/changed__files.zip >> /tmp/o.txt")
      
      (let ( (cmd (string-append "cd /tmp ; zip -0 -r original_files.zip " cmd-old " >> /tmp/o.txt")) )
	(display "\n\nZip cmd: ")
	(display cmd)
	(display "\n\n")
	(r cmd))
      (let ( (cmd (string-append "cd /tmp ; zip -0 -r changed__files.zip " cmd-new " >> /tmp/o.txt")) )
	(display "\n\nZip cmd: ")
	(display cmd)
	(display "\n\n")
	(r cmd))
      
      (display "\n\nChanged files:\n\n")
      (let loop ((files-new files-new))
	(if (not (eqv? '() files-new))
	    (begin
	      (display (string-append "    [TREE]/" (substring (car files-new) 9 (string-length (car files-new))) "\n"))
	      (loop (cdr files-new)))))
      (display "\n\nDone! See /tmp/original_files.zip and /tmp/changed__files.zip now.\n\n\n"))))

(define code-review
  (lambda ()
    (display "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nCode review time!\n\n\n\n")
    (let ((clean-tree (get-clean-tree)))
      (display "\n\nErasing old data...\n")
      (r (string-append "yes | rm -Rf /tmp/tree.old"))
      (r (string-append "yes | rm -Rf /tmp/tree.new"))
      (display "\nNow making first copy...\n")
      (r (string-append "cp -Rf " clean-tree " /tmp/tree.old"))
      (display "\nNow making the second copy...\n")
      (r (string-append "cp -Rf /tmp/tree.old /tmp/tree.new"))
      (display "\n\n\n\nNOW SAVE YOUR FILES TO /tmp/tree.new. Then press any key and Enter.\n\n\n")
      (read) ; Discarded
      
      (finish-cr)
      )))

(define (finish-cr)
  (display "\n\n\n\nNow calculating diff... ")
  (cleanup_in "/tmp/tree.new")
  (cleanup_in "/tmp/tree.old")
  (r (string-append "cd /tmp ; diff -N -b -B --minimal -w -r -U 8 tree.old tree.new > m.diff ; dos2unix m.diff ; unix2dos m.diff"))
;  (r (string-append "cd /tmp ; diff -b -B --minimal -w -r -U 8 tree.old tree.new > m.diff ; dos2unix m.diff ; unix2dos m.diff"))
  (display "...done! See /tmp/m.diff now! ")
  (generate-zips))

;(define l (rglob "/Igor/spsigor_data/tree"))
;(define jsp-files (filter (gen-rx-filter (rx ".jsp" eos)) l))
;(define js-files (filter (gen-rx-filter (rx ".js" eos)) l))
;(define xml-files (filter (gen-rx-filter (rx ".xml" eos)) l))
;(matching-files (rx "manualResolve()") js-files)
(define (good-extensions-filter p)
  (or 
      (regexp-search? (uncase (rx ".java" eos)) p)
      (regexp-search? (uncase (rx ".jsp" eos)) p)
      (regexp-search? (uncase (rx ".properties" eos)) p)
      (and (regexp-search? (rx ".xml" eos) p) (not (regexp-search? (rx "ignoreThisFile.xml" eos) p) ))
      (and (regexp-search? (uncase (rx ".XML" eos)) p) (not (regexp-search? (uncase (rx "ignoreThatFile.bla" eos)) p) ))
      (regexp-search? (uncase (rx ".js" eos)) p)
      (regexp-search? (uncase (rx ".css" eos)) p)
      ))
(define good-extensions-filter (memoize good-extensions-filter))
;.java .jsp .properties .xml .js .css

;(define dev-files (filter good-extensions-filter (rglob "/Igor/spsigor_data/tree")))

; remember (posix-string->regexp string)
; remember (regexp->posix-string re) 

(define (str-regexp-for rx)
  (call-with-values (lambda () (regexp->posix-string rx)) (lambda (a b c d) a)))

; (str-regexp-for numbered-keep-rx)

(define (get-dev-files)
  (filter good-extensions-filter (rglob "/clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/workspace/IGORHVRWebApp")))
(define get-dev-files (memoize get-dev-files))

; (tgrep "123" (get-dev-files))
(define (tgrep str dev-files)
  (define expr (rx ,@str))
  (define mf (matching-files expr dev-files))
  (define (gcmd files)
    (if (eqv? '() files) ""
	(string-append "grep -nH " (str-regexp-for expr)  " " (car files) " ; " (gcmd (cdr files)))))
  (display (r (gcmd mf)))
  #f)

; (define get-dev-padded-files-string (memoize get-dev-padded-files-string))

(define (fgrep str f-list)
  (define expr
    (if (regexp? str) str
	(rx ,@str)))
  (define fname (generate-unique-name "/tmp/fgrep.output"))
  (define files-string (list->space_separated_string (if (eqv? #f f-list) (get-dev-files) f-list)))
  (rt (string-append "echo  \" -*- mode: grep -*- \"" " > " fname " ; "
		     "echo  \"\" >> " fname " ; "
		     "echo \"Matches for " (str-regexp-for expr) ": \" >> " fname " ; "
		     "grep -nH " (str-regexp-for expr)  " " files-string " >> " fname))
  (lbreaks fname)
  (e! fname)
  #t)

(define (grep str dev-files)
  (define expr (uncase (rx ,@str)))
  (matching-files expr dev-files))

; (tgrep "someSearhString")

; (define (xml-files) (filter (lambda (v) (regexp-search? (rx ".xml" eos) v)) (rglob "/Igor/spsigor_data/tree")))

; (for-each (lambda (p) (display "\n") (display p)) (xml-files) )

;(call-with-output-file "response.raw.bytes.dat" (lambda (f)
; Example: (display (integer->char 48) f)
;
;		       ))


(define (fext extension)
  (define fz (rglob "/clear_case_storage_folder"))
  (filter (lambda (e) (regexp-search? (rx ,@extension eos) e)) fz))

(define (dl list)
  (if (eqv? '() list) (display "\n")
      (begin
	(display (car list))
	(display "\n")
	(dl (cdr list)))))



(define (e! files)
  (let ( (final-cmd
	  (if (list? files)
	      (let gcmd ((files files))
		(if (eqv? '() files) ""
		    (string-append "/emacs/bin/emacsclientw.exe -n \"" (car files) "\" ; " (gcmd (cdr files)))))
	      (string-append "/emacs/bin/emacsclientw.exe -n \"" files "\""))) )
    (rt final-cmd)
    #t))

(define (en! files)
  (let ( (final-cmd
	  (if (list? files)
	      (let gcmd ((files files))
		(if (eqv? '() files) ""
		    (string-append "cmd.exe /c start notepad " (car files) " ; " (gcmd (cdr files)))))
	      (string-append "cmd.exe /c start notepad " files))) )
    (rt final-cmd)
    #t))

(define (lbreaks files)
  (let ( (final-cmd
	  (if (list? files)
	      (let gcmd ((files files))
		(if (eqv? '() files) ""
		    (string-append " dos2unix " (car files) " 2> /dev/null ; unix2dos " (car files) " 2> /dev/null ; " (gcmd (cdr files)))))
	      (string-append " dos2unix " files " 2> /dev/null ; unix2dos " files " 2> /dev/null "))) )
    (rt final-cmd)
    #t))

(define l list)


(define (ufl str)
  (apply string (cons (char-upcase (string-ref (symbol->string str) 0)) (cdr (string->list (symbol->string str))))))
  
(define-syntax jb
 (syntax-rules (@)
   ((_ jb-name @ jb-location fields)
    (begin
      (display "\n\npackage ")
      (display 'jb-location)
      (display ";\n\n")
      (display "public class ")
      (display 'jb-name)
      (display " {\n")
      ((lambda elements
         (let it ( (tf (car elements)) )
          (if (eqv? '() tf)
              #f
              (begin
                (display "    private String ")
                (display (car tf))
                (display ";\n")
                (it (cdr tf)))))
         (display "\n")
         (let it ( (tf (car elements)) )
          (if (eqv? '() tf)
              #f
              (begin
                (display "    public String get")
                (display (ufl (car tf)))
                (display "() {\n")
                (display "        return ")
                (display (car tf))
                (display ";\n    }\n")
                (it (cdr tf)))))
         (display "\n")
         (let it ( (tf (car elements)) )
          (if (eqv? '() tf)
              #f
              (begin
                (display "    public void set")
                (display (ufl (car tf)))
                (display "(String ")
		(display (car tf))
		(display ") {\n")
                (display "        this.")
		(display (car tf))
		(display " = ")
		(display (car tf))
                (display ";\n    }\n")
                (it (cdr tf)))))

        ) 'fields)
      (display "}\n")
      (display "\n\n")))))

;(jb BeanName @ com.somecompanysomewhere.xxswa.package (field1 field2 bla blirg))



(define (expressify e)
  (if (regexp? e) e
	(rx ,@e)))

(define (pad-string str)
    (pad-string-with str " "))

(define (pad-string-linebreak str)
    (pad-string-with str "\n"))

(define (pad-string-with str to-pad)
    (string-append str to-pad))

(define (list->space_separated_string l)
  (let ((pl (map pad-string l)))
    (apply string-append pl)))

(define (list->linebreak_separated_string l)
  (let ((pl (map pad-string-linebreak l)))
    (apply string-append pl)))

; Fast function to get matching files.
(define (f-gmf expression flist)
  (let ((expression (expressify expression))
	(fstring (list->space_separated_string flist)))
    (let ( (mstring (r (string-append "grep -nH -l " (str-regexp-for expression)  " " fstring))) )
      (let ((mport (make-string-input-port mstring)))
	(let loop ()
	  (let ((data-line (read-line mport)))
	    (if (eof-object? data-line) '()
		(cons data-line (loop)))))))))

; TODO: Handle file names with spaces and other special characters.
(define n-search
  (lambda (what-to-search-for flist)
    (let ((g-e (car what-to-search-for))
	  (files flist))
      (fgrep g-e (let loop ((other-elements (cdr what-to-search-for)))
		   (if (eqv? '() other-elements) files
		       ; Quicker but buggy.
                       ; (f-gmf (car other-elements) (loop (cdr other-elements)))
                       (matching-files (car other-elements) (loop (cdr other-elements)))
                       ))))))

(define (ts w)
  (n-search (if (list? w) w (list (uncase (expressify w))))(get-dev-files)))
(display "\n\n\n")

;(define-syntax n-s
;  (syntax-rules()
;    ((_ search-list ...) (apply n-search  '(search-list ...)))
;    ))

(define (edit-spsigor-files r)
  (edit-files-with-names-matching-r-in-flist (uncase (expressify r)) (get-dev-files)))
(define etf edit-spsigor-files)

;(define (eaf r)
;  (edit-files-with-names-matching-r-in-flist (uncase (expressify r)) (rglob "/tmp/blist")))

(define (edit-files-with-names-matching-r-in-flist r flist)
  (e! (strings-matching r flist)))

(define (strings-matching r string-list)
  (filter (lambda (f) (not (eqv? f '())))
	  (map
	   (lambda n     
	     (let ( (a-string (car n)) )
	       (if (regexp-search? (expressify r) a-string)
		   (car n)
		   '())))
	   string-list)))
(define strings-matching (memoize strings-matching))

;(define save-files-from-a-into-b
;  (lambda (source destination)
;    (let* ((files-in-source (rglob source))
;           (files-in-destination (rglob destination))
;           (files-with-unique-filename-match (match-by-unique-name source destination)))
;      (map copy-match-into files-with-match)
;      files-with-match)))
;
;(define (copy-match-into pair)
;  (display "\nWould copy ")
;  (display (car pair))
;  (display " into ")
;  (display (cadr pair))
;  (display "."))
;(define (match-by-unique-name list-1 list-2)
;  (let (process-element (lambda (e

(define cic (lambda () (cleanup) (clear_case_storage_folder->igor) (cleanup) "OK! CC store converted to igor and cleaned!"))

(define make-email-tag
  (lambda ()
    (string-append "(e-id " (number->string (random) ) ")")))

(define met make-email-tag)

(define id
  (lambda ()
    (list 'id (random) ) ))

(define (hid)
  (display (string-append "(id " (number->string (random) 16) ")")))

(define (tid) (display (string-append "\n\n&nbsp;&nbsp;&nbsp; [IGORHVR - Task id " (number->string (random) 16) "] \\- \n\n")))

(define (sid) (number->string (id)))

(define (enf r)
  (edit-files-with-names-matching-r-in-flist (uncase (expressify r)) (rglob "/tmp/tree.new")))

(define (tnf r)
  (edit-files-with-names-matching-r-in-flist (uncase (expressify r)) (rglob "/tmp/tree.new")))

(define normalize-priorities
  (lambda (input-numbers)
    (define sorted-nums (binarysort > input-numbers))

    (define al
      (let loop ((data sorted-nums)
		 (n 1))
	(if (eqv? '() data) '()
	    (cons (list (car data) n) (loop (cdr data) (+ n 1))))))
    
    (let loop ((nums input-numbers))
      (if (eqv? '() nums) '()
	  (cons (cadr (assv (car nums) al)) (loop (cdr nums)))))))

(define-syntax negate
  (syntax-rules ()
    ((_ f) (lambda (p) (not (f p))))))

(define (load-property-file fname)
  (define (split-on-newlines string)
    (let ((input-stream (make-string-input-port string)))
      (let build-list ((input-stream input-stream))
        (let ( (element (read-line input-stream) ) )
          (if (eof-object? element) '()
              (cons element (build-list input-stream)))))))
  (define (is-not-property-comment non-empty-str)
    (not (eqv? #\# (string-ref non-empty-str 0))))
  (define (is-not-empty-line str)
    (not (eqv? 0 (string-length str))))
  (define (is-not-useless-definition str)
    (regexp-search? (rx "=") str))
  (define (split-on-equal str)
    (string-split str '(#\=)))
  (let ( (fcontents-string (file->string fname 100000)) )
    (let ( (fcontents-list (split-on-newlines fcontents-string) ) )
      (let ( (trimmed-fcontents (map string-trim fcontents-list) ) )
        (let ( (empty-lines-filtered-fcontents (filter is-not-empty-line trimmed-fcontents)) )
          (let ( (comments-filtered-fcontents (filter is-not-property-comment empty-lines-filtered-fcontents)))
            (let ( (useful-comments-filtered-fcontents (filter is-not-useless-definition          comments-filtered-fcontents))
                   (useless-definitions                (filter (negate is-not-useless-definition) comments-filtered-fcontents))
                   (comments (filter (negate is-not-property-comment) empty-lines-filtered-fcontents)))
              (values (map split-on-equal useful-comments-filtered-fcontents) useless-definitions comments))))))))

(define (sort-alist l)
  (define (cmp-elements e1 e2)
    (string>? (string-upcase (car e1)) (string-upcase (car e2))))
  (binarysort cmp-elements l))

(define find-useless-keys
  (lambda (fname)
    (display "\n\nUseless keys in ")
    (display fname)
    (display ": \n    ")
    (let ((uv (call-with-values (lambda () (load-property-file fname)) (lambda (a b c) b) )))
      (map (lambda (p) (display "\n\n\n -= \n")(write p)(display " =- ")) uv))))

(define (match-properties-keynames f1 f2)
  (define (find-missing d1 d2)
    (define (not-the-truth e)
      (not (eqv? #t e)))
    (let ( (data (if (eqv? '() d1) '()
                     (let ( (v (assoc (caar d1) d2)) )
                       (cons (if (eqv? #f v) (car d1)
                                 #t) (find-missing (cdr d1) d2))))) )
      (filter not-the-truth data)))

  (let ( (unsorted-d1 (call-with-values (lambda () (load-property-file f1)) (lambda (a b c) a) ))
         (unsorted-d2 (call-with-values (lambda () (load-property-file f2))  (lambda (a b c) a))) )
    (let ( (d1 (sort-alist unsorted-d1))
           (d2 (sort-alist unsorted-d2)) )
      (let ( (display-and-break (lambda (v) (display "\n        ") (display v)))
             (only-in-f1 (map car (find-missing d1 d2)))
             (only-in-f2 (map car (find-missing d2 d1))) )
        (display
         (string-append "\n\n\nMatching  " f1 " and " f2 "..."))
        (if (not (eqv? '() only-in-f1))
            (begin
              (display (string-append "\n    Keys present only in " f1 " and NOT in " f2 ": "))
              (map display-and-break only-in-f1)))
        (if (not (eqv? '() only-in-f2))
            (begin
              (display (string-append "\n    Keys present only in " f2 " and NOT in " f1  ": "))
              (map display-and-break only-in-f2)))
        (display "\n\n")))))



(define (property-match)
  (match-properties-keynames "/tmp/x1.properties"  "x2.properties")
  )

(define replace-string-by-string-in-folder
  (lambda (to-replace new-value folder)
    (let* ( (look-for-string to-replace)
	    (look-for (expressify look-for-string))
	    (replacement new-value) )
      (define l (rglob folder))
      (define tmf (matching-files look-for l))
      (display "\n")
      (for-each
       (lambda (f)
	 (display (string-append "\nFound match! Running replace of " look-for-string  " by " replacement  " in file: "))(display f)(display "\n")
	 (display (r (string-append "echo Matches being corrected: ; "
				    "grep -nh " (str-regexp-for look-for) " " f)))
         (replace-in-file f look-for replacement))
       tmf)
      (display "\n\n\n")
      #f)))


(define (gd arg)
  (string-append "\n// TODO: Remove line below. DEBUGGING-ALERT." "\n" "alert('" arg ": ' + " arg ");\n\n"))

(define (gm arg)
  (string-append "\n// TODO: Remove line below. DEBUGGING-MARK." "\n" "alert('DEBUGGING-MARK:\\n" arg "');\n\n"))


(define (generate-debugging-statements n)
  (let loop ((counter 0))
    (if (= n counter) ""
        (string-append (gm
                        ;(cadr (id))
                        (number->string counter)
                           ) (loop (+ counter 1))))))

(define (e-a rx place)
  (define files (rglob place))
  (define mf (matching-files (expressify rx) files))
  (e! mf)
  (values))

(define (get-harvest-server)
  "mhostname"
  )

; Check Out for Browse (Read Only)
(define rebase_to
  (lambda (location)
    (let ((cmd (string-append "cd " location " ; hco -b " (get-harvest-server) " -en \"" (get-harvest-project-name) "\" -st " (get-harvest-state) " -br -r -eh " (get-harvest-credentials-file) " -vp \\\\IGORHVR\\\\sssdsat -nvs ; tail -n 2 hco.log")))
      (d "\nRemoving old workspace... ")
      (r (string-append "rm -Rf " location "/workspace"))
      (d "done. \n")
      (display (string-append "Will now run \n" cmd "\n\n"))
      (r cmd)
      (classpath location)
      (copy-model location)
      (gitignore location)
      (rt (string-append "cd " location " ; attrib /s -R > nul"))
      (display "\nRebase is done!\n")
      )))

(define rebase (lambda () (rebase_to "/clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat")))
;(define rebase
;  (lambda ()
;    (let ((cmd (string-append "cd /clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/ ; hco -b " (get-harvest-server) " -en \"" (get-harvest-project-name) "\" -st " (get-harvest-state) " -br -r -eh " (get-harvest-credentials-file) " -vp \\\\IGORHVR\\\\sssdsat -nvs ; tail -n 2 hco.log")))
;      (d "\nRemoving old workspace... ")
;      (r "rm -Rf /clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/workspace")
;      (d "done. \n")
;      (display (string-append "Will now run \n" cmd "\n\n"))
;      (r cmd)
;      (classpath)
;      (copy-model)
;      (gitignore)
;;      (r "cd /clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/ ; date > last_rebase ")
;      (rt "cd /clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/ ; attrib /s -R > nul")
;      (display "\nRebase is done!\n")
;      )))

(define (classpath . p)
  (let ((location (if (eqv? '() p) "/clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat" (car p))))
    (r (string-append "rm -f " location "/workspace/IGORHVRWebApp/.classpath ; cp /Igor/i/IGORHVRWebApp_good_dot_classpath " location "/workspace/IGORHVRWebApp/.classpath"))))


; * Works in current directory always. Assuming current directory is sssdsat... *
; 
; 
; -nvs Specifies what will be checked out.
; -eh Credentials file. Created with the command svrenc.exe -f harvest_credentials -dir /Igor/i
; -st state = Dev
; -vp view path
; -b - harvest machine
; -en "project"
; -br checkout for browse (read only)
; -r overwrite read only files (to update a previous check out)
; -prompt prompt for the password

(define (dm)
  (string-append "\n\n// TODO Remove this DEBUGGING MESSAGE.\n"
                 "System.err.println(\"Debugging point " (number->string (cadr (id))) ".\");\n\n"))

(define execute-n-times
  (lambda (n thunk)
    (if (> n 0)
      (begin
        (thunk)
        (execute-n-times (- n 1) thunk)))))

(define-syntax times
  (syntax-rules ()
    ((_ n code ...) (execute-n-times n (lambda () code ...)))
    ))


;;; Harvest crap.

(define get-harvest-project-name)
(define set-harvest-project-name)

(let ((r #f))
  (set! get-harvest-project-name
        (lambda ()
          (d "\n\n(get-harvest-project-name) ; ---> " r "\n\n")
          r))
  (set! set-harvest-project-name
        (lambda (p)
          (set! r p)
          (d "\n\n(set-harvest-project-name \"" p "\") ; ---> DONE. \n\n")
          r))
  )

(set-harvest-project-name "IGORHVR-XA0331")

(define get-harvest-state)
(define set-harvest-state)

(let ((r #f))
  (set! get-harvest-state
        (lambda ()
          (d "\n\n(get-harvest-state) ; ---> " r "\n\n")
          r))
  (set! set-harvest-state
        (lambda (p)
          (set! r p)
          (d "\n\n(set-harvest-state \"" p "\") ; ---> DONE. \n\n")
          r))
  )
(set-harvest-state "Dev")
;(set-harvest-state "Integration")

;(define (get-harvest-state)
;  "Integration"
;  ;"Dev"
;  )

(define *current-harvest-package*   "IGORHVR-XYX0416-igorhvr_test_package_ignore")
(define (get-harvest-package-name)
  *current-harvest-package*
  )


  
  

(define (get-harvest-credentials-file)
  "/Igor/i/harvest_credentials")

(define (get-my-racfid) "igorhvr")


(define get-migration-source)(define set-migration-source)
(let ((r #f))
  (set! get-migration-source  (lambda () r))
  (set! set-migration-source  (lambda (p) (set! r p)  r)))

(define get-migration-destination)(define set-migration-destination)
(let ((r #f))
  (set! get-migration-destination  (lambda () r))
  (set! set-migration-destination  (lambda (p) (set! r p)  r)))


(define (migrate package-name)
  (do-migrate "Integration" (get-migration-source) package-name
              "Dev" (get-migration-destination) (string-append "Migration_" package-name)))

(define (migrate-dev package-name)
  (do-migrate "Dev" (get-migration-source) package-name
              "Dev" (get-migration-destination) (string-append "Migration_" package-name)))

(define (do-migrate source-state source-project source-package
                    destination-state destination-project destination-package)  
  (create-harvest-package-with-name-in destination-package destination-project)
  
  (execute-harvest-cross-project-merge 'merge-aggressively
                                         source-project source-state source-package
                                         destination-project destination-state destination-package
                                         (get-harvest-server)
                                         (get-harvest-credentials-file)))
    

(define execute-harvest-cross-project-merge
  (lambda (merge-option source-project source-state source-package destination-project destination-state destination-package server credentials-file)
    (define (get-id-for-merge-option merge-option)
      (case merge-option
        ('merge-conservatively "1")
        ('merge-aggressively "2")
        ('take-target-trunk-version "3")
        ('take-source-branch-version "4")
        (else (raise-error))
        ))
    (let ((where (create-temp-directory)))
      (let ((cmd (string-append "cd " where " ; " "hcropmrg "  " -b " server " -mo " (get-id-for-merge-option merge-option) " "
                 " -en2 " source-project      " -st2 " source-state      " -p2 \"" source-package "\""
                 " -en1 \"" destination-project "\" -st1 " destination-state " -p1 \"" destination-package "\" "
                        " -eh " credentials-file " ; cat hcropmrg.log")))
        (d "\nWill run: " cmd)
        (let ((results (r cmd)))
          (d "\n=> " results)
          results)))))



; Sample command hcp IGORHVR-TST-igorhvr-description -b mhostname -en IGORHVR-TST -st Dev -eh /Igor/i/harvest_credentials
(define execute-create-harvest-package
  (lambda (package server project state credentials-file)
    (let ((where-to-checkout (create-temp-directory)))
      (r (string-append "cd " where-to-checkout " ; " "hcp \"" package "\" -b " server " -en " project " -st " state " -eh " credentials-file " ; cat hcp.log")))))

(define (create-harvest-package package-description)
  (let ((new-package-name (string-append (get-harvest-project-name) "-" (get-my-racfid) "-" package-description)))
    (create-harvest-package-with-name-in new-package-name (get-harvest-project-name))))

(define (create-harvest-package-with-name-in package-name project-name)
  (d "\nCommand to create package with name " package-name " will be issued.\n")
  (execute-create-harvest-package package-name
                                  (get-harvest-server)
                                  project-name
                                  "Dev" ; could be (get-harvest-state)
                                  (get-harvest-credentials-file)
                                  )
  (set! *current-harvest-package* package-name))

(define (fname->hname t)
  (let ((tl (reverse (split-file-name t))))
    (list (fold (lambda (v1 v2) (string-append "\\\\" v1 v2)) "" (cdr tl))
          (car tl))))

(define (local_fname->hname t)
  (list (string-append "\\\\IGORHVR\\\\sssdsat" (car (fname->hname t))) (cadr (fname->hname t))))

(define execute-harvest-checkout 
  (lambda (where-to-checkout server project state process-name checkout-type credentials-file view-path package-name file-name flags)
    (let ((cmd
           (string-append
            "cd " where-to-checkout " ; " "hco -b " server " -en " project " -st " state " -pn \"" process-name "\" -eh " credentials-file " -vp " view-path " -p \"" package-name "\" -" checkout-type " " file-name " " flags " ; cat hco.log ")))
      (d "\n\nWill execute checkout command: " cmd "\n\n")
;      (r cmd))))
      (let ((result (r cmd))) (d "\n" result "\n") result))))


(define (get-delivery-root)  "/clear_case_storage_folder/KDJFSLDFSA/KDJFSLDFSA/KDSFLKDSJFLSKDJFS/sssdsat/")

(define deliver
  (lambda (filename hlocation hname commit-comment new-file? prevent-overwrites?)
    (let* ((tmpdir (create-temp-directory))
           (tf (string-append tmpdir "/" hname))
           (rfname (string-append (get-delivery-root)
                                   filename))
           (rebase-tree-fname (string-append "/rebase/"
                                   filename))
           )
      (unless new-file?
        (harvest-checkout-dev tmpdir hlocation hname)
        (when prevent-overwrites?
          (d "\n\nDeliver - Comparing " tf " to " rebase-tree-fname "... ")        
          (unless (equal-files? tf rebase-tree-fname)
            (error "\n\n\n\nYOU ARE ABOUT TO OVERWRITE CHANGES YOU HAVE NEVER SEEN! STOPPING RIGHT HERE.\n (You can use (set-prevent-overwrites #f) to override this verification.) \n\n\n\n"))
          (d "OK. Going ahead.\n"))
        )
      (r (string-append "rm -f " tf ))
      (r (string-append "cp " rfname " " tf))
      (harvest-checkin-dev  tmpdir hlocation hname commit-comment new-file?)
      )))

(define get-prevent-overwrites)
(define set-prevent-overwrites)

(let ((r #t))
  (set!  get-prevent-overwrites
        (lambda ()          
          (unless r (d "\n\n\nATTENTION: NOT PREVENTING OVERWRITES.\n(Use (set-prevent-overwrites #t) to fix this)\n\n\n"))
          r))
  (set! set-prevent-overwrites
        (lambda (p)
          (set! r p)
          (d "\n\n(set-prevent-overwrites \"" p "\") ; ---> DONE. \n\n")
          r))
  )


(define qdeliver
  (lambda param-list ;(fname commit-comment <optionally new-file?>)
    (let ((new-file? (cond 
                      ((= 3 (length param-list)) (list-ref param-list 2))
                      (else #f)))
          (fname (list-ref param-list 0))
          (commit-comment (cond 
                           ((> 1 (length param-list)) (list-ref param-list 1))
                           (else (get-harvest-package-name))))
          )
      
      (let ((hn (local_fname->hname fname)))
        (let (
              (hlocation (car hn))
              (hname (cadr hn))
              )
          (deliver fname hlocation hname commit-comment new-file? (get-prevent-overwrites))          
          )))))

(define execute-harvest-checkin
  (lambda (where-to-checkin server project state process-name credentials-file view-path package-name file-name commit-comment flags)
    (let ((cmd
           (string-append
            "cd " where-to-checkin " ; " "hci " file-name  " -b " server " -en " project " -st " state " -pn \"" process-name "\" -eh " credentials-file " -vp " view-path " -p \"" package-name "\" -de \"" commit-comment "\" -op as " flags " ; cat hci.log ")))
      (d "\n\nWill execute checkin command: " cmd "\n\n")
      (let ((result (r cmd))) (d "\n" result "\n") result))))


(define harvest-checkin-dev
  (lambda (where-to-checkin hlocation hname commit-comment new-file?)
    (execute-harvest-checkin
                            where-to-checkin
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Dev"
                            (if new-file? "Check In New Item(s)" "Check In")
                            (get-harvest-credentials-file)
                            hlocation ; Sample value: "\\\\IGORHVR\\\\sssdsat\\\\workspace\\\\IGORHVRWebApp\\\\WebContent"
                            (get-harvest-package-name)
                            hname ; Sample: "index.jsp"
                            commit-comment
                            "" ; No flags
                            )))


(define harvest-checkout-dev
  (lambda (where-to-checkout hlocation hname)
    (execute-harvest-checkout
                            where-to-checkout
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Dev"
;                            "Check Out on Trunk"
                            "Check Out on Branch"
;                            "up" ; Use "ro" for not bringing file.
                            "cu"
                            (get-harvest-credentials-file)
                            hlocation ; Sample value: "\\\\IGORHVR\\\\sssdsat\\\\workspace\\\\IGORHVRWebApp\\\\WebContent"
                            (get-harvest-package-name)
                            hname ; Sample: "index.jsp"
                            "" ; No flags
                            )))

(define copy-xax
  (lambda p
    (let ((location (if (eqv? '() p) "/clear_case_storage_folder/sssdsat" (car p))))
      (d "\n\nCopying model from\n"
         "    " location "/workspace/xax/" "\n"
         " to\n"
         "    " location "/workspace/xax/" "\n\n"
         (r (string-append "cp -Rfa " location "/workspace/xax/* " location "/workspace/xax/"))))))
  
(define (start arg)
  (rt (string-append "cmd /c start " arg))
  (values))

(define (igorhvr)
  (start "https://www.iasylum.net/"))

(define (release-notes)
  (e! "/clear_case_storage_folder/releasenotes.jsp"))

(define (gitignore . p)
  (let ((location (if (eqv? '() p) "/clear_case_storage_folder/" (car p))))
    (r (string-append "cd " location " ; git checkout workspace/.gitignore workspace/f1/.gitignore"))))

(define (status) (r (string-append
      "cd /clear_case_storage_folder/ ; "
      "git status ; ")))

(define (hours) (start "http://somewhere:8080"))

(define (kill-matches re s)
  (regexp-substitute/global #f re s 'pre 'post))

(define kill-before-slash
  (lambda (str)
    (kill-matches (rx (: (* any) "/")) str)))

(define kill-after-at
  (lambda (str)
    (kill-matches (rx (: "@" (* any))) str)))

(define grab-stddocs-templates
  (lambda ()
    (define current-location (create-temp-directory))
    (define retrieve
      (lambda (url)
        (r (string-append "wget '" url "'"))))
    (define file_listing #f)
    (define f_suffixes #f)
    (define urls #f)
    (define files_to_process #f)    
    (with-cwd
     current-location

     (d "\n\nWill store updated STDDOCS templates at " current-location " ...\n")
     
     (r "wget \"http://somecompanysomewhere.com/\" -O doc_listing")
     
     (set! file_listing (file->string "doc_listing" 1000000))
     
     (set! f_suffixes '())
     (regexp-for-each
      (uncase (rx  (+ (: "href=\"" (* (- any "\""))  "\""))))
      (lambda (p)
        (set! f_suffixes (cons
                         (substring (match:substring p)
                                    6
                                    (- (string-length (match:substring p)) 1)
                                    )
                         f_suffixes))
        )
      file_listing 0)     

     (set! urls (map (lambda (v) (string-append "http://msomecompanysomewhere.com" v)) f_suffixes))
     (map retrieve urls)

     (set! files_to_process (rglob current-location))

     (for-each
      (lambda (f)
        (let ((fname (kill-before-slash f)))
          (let ((new-fname (kill-after-at fname)))
            ;(d "\n(with-cwd \"" current-location "\" (rename-file \"" fname "\" \"" new-fname "\"))\n")
            (with-cwd current-location (rename-file fname new-fname))
            ;(with-errno-handler* (lambda err #f) (lambda () (rename-file fname new-fname)))
            )))
      files_to_process)
     (delete-file "doc_listing")
     (r (string-append "cd " current-location " ; cmd /c start ."))
     )))

; (regexp->posix-string (rx (seq digit digit "/" digit digit "/" digit digit digit digit (or "|" " ") (or (seq digit digit ":" digit digit ":" digit digit) (seq digit digit ":" digit digit)) (or "|" " ") (* (- any ("| "))) ("| "))))
;"[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9][ |]([0-9][0-9]:[0-9][0-9]:[0-9][0-9]|[0-9][0-9]:[0-9][0-9])[ |][^ |]*[ |]"

;(regexp->posix-string
; (rx (seq digit digit "/" digit digit "/" digit digit digit digit
;          (or "|" " ")
;          (or (seq digit digit (or ":" ".") digit digit (or ":" ".") digit digit) (seq digit digit (or ":" ".") digit digit))
;          (or "|" " ")
;          (* (- any ("| ")))
;          ("| "))))
;"[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9][ |]([0-9][0-9][.:][0-9][0-9][.:][0-9][0-9]|[0-9][0-9][.:][0-9][0-9])[ |][^ |]*[ |]"

;(regexp->posix-string
; (rxw
;  (or
;   (seq digit digit "/" digit digit "/" digit digit digit digit
;        (or "|" " ")
;        (seq digit digit (or ":" ".") digit digit (? (seq (or ":" ".") digit digit )))
;        (or "|" " ")
;        (* (- any ("| ")))
;        ("| "))
;   (seq "Some kind of thing to look for  "
;        digit digit "/" digit digit "/" digit digit digit digit
;        " by WHOKNOWSWHO: " (= 6 (/"AZaz09")) " ")
;   (seq "bla at:" (** 1 2 "\n") "Somecompanysomewhere")
;   (seq "ble to:" (** 1 2 "\n") "Somecompanysomewhere")
;   )))

(define ignore (lambda p (if #f 'never-returned)))

(define (email tn td)
  (begin
  (write
  ;(ignore 
   (let ((comments "???")
      (ticket-number tn)
      (issue-description td)
      (who "Igor")
      (email-id (met)))    
(string-append "\n\n\nTo: AHA " comments
                              "\nCc: person1, person2 \n"
                             "\nSubject: URGENT ISSUE " ticket-number ".\n"
                             "\nHello!\n"
                             "\n   About ticket " ticket-number ": " issue-description "\n"                             
                             "\n      * ===\n"
                             "\nRegards,"
                             "\n   " who "."
                             "\n" email-id "\n\n\n")))
))

(define (scsh-manual)
  (start "http://www.scsh.net/docu/html/man-Z-H-7.html#node_chap_6"))

;(regexp->posix-string (rx   (seq (submatch (| "GET " "POST ")) (submatch (+ (- any (" ")))) (submatch (seq " HTTP" (* any))))))
;(regexp->posix-string (rx "com.somecompanysomewhere"))

;(regexp->sre (posix-string->regexp "^([a-zA-Z]+|[a-zA-Z]+[ a-zA-Z]+[a-zA-Z]+)$"))
;
;(regexp->posix-string (sre->regexp '
;                       (: bos
;                          (|
;                           (+ (/ "AZaz"))
;                           (seq (+ (/ "AZaz")) (+ (|(/ "AZaz") (" ")))  (+ (/ "AZaz"))))
;                          eos)))
; (regexp->posix-string (rx "autocomplete=off \">"))

(define dos2unix (lambda (filename) (r (string-append "dos2unix " filename))))
(define unix2dos (lambda (filename) (r (string-append "unix2dos " filename))))

(define equal-files?
  (lambda (file-a file-b)
    (define diff-result (r (string-append "if diff -U 8 " file-a " " file-b " > /tmp/equal-files-diff-result.txt ; then echo -n EQUAL ; else echo -n DIFFERENT ; fi")))
    
    (cond 
     ((string=? "EQUAL" diff-result) #t)
     ((string=? "DIFFERENT" diff-result) #f)
     (else (error "Error comparing files " file-a " and " file-b ": " diff-result "\n\n" (r "cat /tmp/equal-files-diff-result.txt") )))))

(define (gr)
  (with-cwd "/rebase"
            (rebase_to "/rebase")
            (d (r "git pull > /tmp/rebaser_outputs 2>&1 ; git status >> /tmp/rebaser_outputs 2>&1 ; git add . >> /tmp/rebaser_outputs 2>&1 ; git commit -a -m Rebase >> /tmp/rebaser_outputs 2>&1 ; git push >> /tmp/rebaser_outputs 2>&1 ; cat /tmp/rebaser_outputs"))))

(define rebaser
  (lambda ()
    (define not-there (lambda () (display "\n\nFile /rebase/CONTINUE_REBASER is not there. Stopping auto-rebaser process...\n\n")))
    (with-cwd "/rebase"
              (if (file-exists? "CONTINUE_REBASER")
                  (begin
                    (d "\n\nFile CONTINUE_REBASER is there - doing a rebase now.\n\n")
                    (gr)
                    (if (file-exists? "CONTINUE_REBASER")
                        (begin (d "\nWaiting 5 minutes now, before restarting process.\n")
                               (r "sleep 300")
                               (rebaser)
                               )
                        (not-there)
                        ))
                  (not-there)))))


; (define s (socket-connect protocol-family/internet socket-type/stream "mcfr1n03" 80))
; (define first-request
; "GET http://hey HTTP/1.1
; Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*
; Accept-Language: en-us
; User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727; InfoPath.1) Paros/3.2.13
; Host: machine.somecompanysomewhere.com
; 
; ")
; (write-string first-request (socket:outport s))
; (define first-response (read-string 1000000 (socket:inport s)))
; 
; (define get-hidden-field
;   (lambda (field-name str)
;     (define gen-field-rx (lambda (field-name) (rx (seq "<input name=\"" ,@field-name "\" type=\"hidden\" value=\"" (submatch (* (- any "\""))) "\">"))))
;     (define e (gen-field-rx field-name))
;     (let ( (md (regexp-search e first-response)) )
;       (if md (match:substring md 1)
;           #f))))
; 
; (define URLBack (get-hidden-field "URLBack" first-response))

(set-harvest-project-name "IGORHVR-XA5611")

(set-harvest-state "Dev") ; ---> DONE.


(begin
(set-migration-source "IGORHVR-XA5611")(set-migration-destination "IGORHVR-XA5162")
)
;(begin  (set-migration-source "IGORHVR-XA0901")(set-migration-destination "IGORHVR-XA0818") )


(define (qd s)
  (define make-qd-rx (lambda (type) (rx (: ,@type (+ whitespace) (submatch (+ (~ whitespace))) (* whitespace) (? "\n")))))
  (define modified-rx (make-qd-rx "modified:"))
  (define new-rx (make-qd-rx "new file:"))

  (d "\n\n")
  (regexp-for-each modified-rx (lambda (m) (define v (match:substring m 1)) (d "\n(qdeliver \"" v "\")")) s )  
  (d "\n")
  (regexp-for-each new-rx (lambda (m) (define v (match:substring m 1)) (d "\n(qdeliver \"" v "\" (get-harvest-package-name) #t)")) s )  
  (d "\n\n")
  (regexp-for-each modified-rx (lambda (m) (define v (match:substring m 1)) (qdeliver v                              )) s )
  (d "\n")
  (regexp-for-each new-rx      (lambda (m) (define v (match:substring m 1)) (qdeliver v (get-harvest-package-name) #t)) s )
  (d "\n\n")
  (hd)
  )


;;===

(define execute-harvest-package-concurrent-merge
  (lambda (where server project state process-name credentials-file package-name merge-option flags)
    (define (get-flag-for-merge-option merge-option)
      (case merge-option
        ('merge-conservatively " -mc ")
        ('merge-aggressively " -ma ")
        ('take-target-trunk-version " -tt ")
        ('take-source-branch-version " -tb ")
        (else (raise-error))))
        
    (let ((cmd
           (string-append
            "cd " where " ; " "hccmrg -b " server " -en " project " -st " state " -pn \"" process-name "\" -eh " credentials-file                                         " -p \"" package-name "\" " (get-flag-for-merge-option merge-option)  " " flags " ; cat hccmrg.log ")))
      (d "\n\nWill execute checkin command: " cmd "\n\n")
      (let ((result (r cmd))) (d "\n" result "\n") result))))



(define harvest-package-concurrent-merge
  (lambda (package-name)
    (execute-harvest-package-concurrent-merge
                            (create-temp-directory)
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Dev"
                            "Concurrent Merge"
                            (get-harvest-credentials-file)
                            package-name
                            'merge-conservatively
                            "" ; No flags
                            )))

;;hap package_name1 package_name2... {-b name -en name -st name} [-v] [-pn name] [-c text] {-usr username -pw password} [-rej] [-prompt] [-i inputfile.txt | -di inputfile.txt] [-eh filename] [-o filename | -oa filename] [-arg] [-wts] [-h]
(define execute-harvest-package-approval
  (lambda (where what-to-do server project state process-name credentials-file package-name flags)
       
    (let ((cmd
           (string-append
            "cd " where " ; " "hap \"" package-name  "\" -b " server " -en " project " -st " state " -pn \"" process-name "\" -eh " credentials-file                                    (if what-to-do " " " -rej ")     " " flags " ; cat hap.log ")))
      (d "\n\nWill execute command: " cmd "\n\n")
      (let ((result (r cmd))) (d "\n" result "\n") result))))



(define harvest-package-approval
  (lambda (what-to-do package-name)
    (execute-harvest-package-approval
                            (create-temp-directory)
                            what-to-do
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Dev"
                            "Approve for Promotion to Integration"
                            (get-harvest-credentials-file)
                            package-name
                            "" ; No flags
                            )))



(define execute-harvest-package-promotion
  (lambda (where server project state credentials-file package-name flags)
       
    (let ((cmd
           (string-append
            "cd " where " ; " "Hpp \"" package-name  "\" -b " server " -en " project " -st " state " -pn \"" "Promote to Integration" "\" -eh "                         credentials-file  " -pm -pd " flags " ; cat Hpp.log ")))
      (d "\n\nWill execute command: " cmd "\n\n")
      (let ((result (r cmd))) (d "\n" result "\n") result))))

(define harvest-package-promotion
  (lambda (package-name)
    (execute-harvest-package-promotion
                            (create-temp-directory)
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Dev"
                            (get-harvest-credentials-file)
                            package-name
                            "" ; No flags
                            )))

(define execute-harvest-package-demotion
  (lambda (where server project state credentials-file package-name flags)
       
    (let ((cmd
           (string-append
            "cd " where " ; " "hdp \"" package-name  "\" -b " server " -en " project " -st " state " -pn \"" "Demote to Dev" "\" -eh "                         credentials-file  " -pd " flags " ; cat hdp.log ")))
      (d "\n\nWill execute command: " cmd "\n\n")
      (let ((result (r cmd))) (d "\n" result "\n") result))))

(define harvest-package-demotion
  (lambda (package-name)
    (execute-harvest-package-demotion
                            (create-temp-directory)
                            (get-harvest-server)
                            (get-harvest-project-name)
                            "Integration"
                            (get-harvest-credentials-file)
                            package-name
                            "" ; No flags
                            )))
(define hd
  (lambda p
    (let ((package-name (if (eqv? '() p) (get-harvest-package-name) (car p))))
      (d "\nMerging & approving package (package \"" package-name "\") ...\n")
      (harvest-package-concurrent-merge package-name)
      (harvest-package-approval #t package-name))))
      ;(harvest-package-promotion package-name)

(define (rd np)
  (let ((n (if (number? np) (number->string np) np)))
    (create-harvest-package (string-append "Build_" n "_release_notes_update"))
    (qdeliver "workspace/IGORHVRWebApp/rdu-index.jsp")
    (hd)))

(define (dmig name) (set-migration-source "IGORHVR-XA0901") (set-migration-destination "IGORHVR-XA0915") (migrate name) (set-migration-destination "IGORHVR-XA1001") (migrate name))

