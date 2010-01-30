(require (lib "defmacro.ss"))
(define-syntax function-for 
  (syntax-rules ()
    ((_ function-symbol-name) (eval function-symbol-name (interaction-environment)))))

(define (sread sdata)
  ( 
   (function-for (car sdata)) 
   (cdr sdata)))

(define-syntax TODO
  (syntax-rules ()
    ((_ function-name function-name-symbol)
      (define function-name 
       (lambda p 
         (string-append "\nTODO - " (symbol->string (quote function-name-symbol)) " - TODO\n"))))
    ((_ function-name)
     (TODO function-name function-name))
    ))

(define tree-sread
  (lambda (params)
    (apply string-append (map sread params))))

(TODO todo)
(define (to-string v)
  (let ( (result (open-output-string)) )
    (display v result)
    (get-output-string result)))
; ===


(define (import imported-class) (string-append "import " (car imported-class) ";\n"))

(define (package package-name) (string-append "\n\npackage " (car package-name) ";\n"))

(define java-file tree-sread)

(define-syntax get-param 
  (syntax-rules ()
    ((_ pname list) (cdr (assv 'pname list)))))

(define ac
  (lambda (p)
    (if (eqv? '() p) ""
        (string-append (symbol->string (car p)) " " (ac (cdr p))))))

(define singleton-constructor #f)


(define (method m-params)
  (apply
   (lambda (ac-data return-value method-name param-list exception code)
     (string-append
      (ac (cdr ac-data))
      return-value
      " "
      method-name
      "("      
       (let loop ( (param-list param-list) )
	 (if (eqv? '() param-list) ""
	     (string-append (car param-list)
			    (let ((rest (cdr param-list)))
			      (if (eqv? rest '()) ""
				  (string-append 
				   ", "
				   (loop (cdr param-list))))))))
	  	  
      ")"
      " "
      exception
      " {\n"
      (sread code)
      "}\n")) m-params))
  
(define jcode
  (lambda params
    (apply string-append (car params))))

(define class
  (lambda plist
    (let ( 
	  (class-name (car (get-param name (car plist))))
	  (ac-data (get-param ac (car plist)))
	  (extends (assv 'extends (car plist)))
	  )
      (set! singleton-constructor (lambda p (string-append "private " class-name "() {\nsuper();\n}")))
      (string-append (ac ac-data)
                     "class " class-name 
		     (if (eqv? #f extends) ""
			 (string-append " extends " (cadr extends) ))
		     " { \n"
		     (tree-sread (cdr (assv 'body (car plist))))
		     "}\n"))
    ))

(define data 
  '(java-file (package "pkg.a")
	      (import "pkg.PropClass")
	      (import "pkg")
	      (class (name "Sample")
		     (extends "TheSuperClass")
		     (ac public)
		     (body
		      (singleton-constructor)
		      (method (ac public static)
			      "SampleResponse" "doWork" ( "String param1" "String p2" "String p3" ) "throws Exception"
			      (jcode
			       "Sample" "Request request = new " "Sample" "Request();"        
			       "request.set" "Param1"  "(" "param1" ");"
			       "System.setProperty(\"" "SampleUrlAlias" "\", getUrl());"
			       "final " "Sample" "Response result = (" "Sample" "Response) TheSuperClass.run(request);"
			       "return result;"))
		      (method (ac private static)
			      "String" "getUrl" ()
			      (jcode
			       "String result=PropClass.getProperty(\"" "SampleUrlAlias" "\");"
			       "return result;"))))))

;(display (sread data))
			
	      