(define break-on-newlines
  (lambda (str)
    (let ((result (let ((chars (string->list str)))
      (let outer-loop ((chars chars))        
        (let ((tl (let loop ((chars chars))
                    (if (eqv? '() chars) '()
                        (if (not (eqv? #\newline (car chars)))
                            (cons (car chars) (loop (cdr chars)))
                            (list (cdr chars))
                            )))))          
          (if (eqv? '() tl) '()
              (if (list? (car (reverse tl))) (cons (list->string (reverse (cdr (reverse tl)))) (outer-loop (car (reverse tl))))
                  (cons (list->string tl) '()))))))))
      (if (string? result)
          (list result)
          result ))))

(define indent
  (lambda (str)
    (let ((indent-single-line (lambda (l) (string-append "    " l "\n"))))
      (apply string-append
             (map indent-single-line (break-on-newlines str))))))

(define gen-init
  (lambda (d)
    (define p
      (lambda (data)
	(string-append (cadr data) " ")))
    (string-append "initializations from \"" input-file-name "\"" "\n"
		   (indent (apply string-append (map p d)))
		   "end-initializations\n\n")))

(define gen-decl
  (lambda (d)
    (define p
      (lambda (data)
	(cond ((eqv? 'number (car data)) (string-append (cadr data) ": " "real\n") )
	      ((eqv? 'set (car data)) (string-append (cadr data) ": " "set of string\n") )
	      ((eqv? 'alist (car data)) (string-append (cadr data) ": " (caddr data) "\n") )

; PROCESSAMENTO: array (TAREFAS) of real;
; (alist "PESO" "array (TAREFAS) of real")

	      (else (error)))
	))
    (string-append "declarations" "\n"
		   (indent (apply string-append (map p d)))
		   "end-declarations\n\n")))

(define standard-bs
  (string-append
   "\n\n\nmodel " model-name "\n\n"
   "uses \"mmxprs\";\n\n"))

(define suffix
  (string-append
 "\n"
"writeln(\"Start.\")" "\n"
"writeln(\"-End.-\")" "\n"
"end-model" "\n" "\n"))


(define g
  (lambda (p)
    (display (string-append
	      standard-bs	      
	      (gen-decl p)
	      (gen-init p)
	      suffix
	      ))))
(define generate-mosel-code g)