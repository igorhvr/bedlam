(load "myenv-sisc.scm")
(import string-io)
(import ssax)
(import util)

(run-test
 (assert (eq? '_
		 (call-with-input-string "_" SSAX:read-NCName)))
 (assert (eq? '_
		 (call-with-input-string "_" SSAX:read-QName)))
 (assert (eq? (string->symbol "_abc_")
	      (call-with-input-string "_abc_;" SSAX:read-NCName)))
 (assert (eq? (string->symbol "_abc_")
	      (call-with-input-string "_abc_;" SSAX:read-QName)))
 (assert (eq? (string->symbol "_a.b")
	      (call-with-input-string "_a.b " SSAX:read-QName)))
 (assert (equal? (cons (string->symbol "_a.b") (string->symbol "d.1-ef-"))
	      (call-with-input-string "_a.b:d.1-ef-;" SSAX:read-QName)))
 (assert (equal? (cons (string->symbol "a") (string->symbol "b"))
	      (call-with-input-string "a:b:c" SSAX:read-QName)))

 (assert (failed? (call-with-input-string ":abc" SSAX:read-NCName)))
 (assert (failed? (call-with-input-string "1:bc" SSAX:read-NCName)))
)

(run-test
 (assert (eq? '= (name-compare 'ABC 'ABC)))
 (assert (eq? '< (name-compare 'ABC 'ABCD)))
 (assert (eq? '> (name-compare 'XB 'ABCD)))
 (assert (eq? '> (name-compare '(HTML . PRE) 'PRE)))
 (assert (eq? '< (name-compare 'HTML '(HTML . PRE))))
 (assert (eq? '= (name-compare '(HTML . PRE) '(HTML . PRE))))
 (assert (eq? '< (name-compare '(HTML . PRE) '(XML . PRE))))
 (assert (eq? '> (name-compare '(HTML . PRE) '(HTML . P))))
 (assert (eq? '< (name-compare '(HTML . PRE) SSAX:largest-unres-name)))
 (assert (eq? '< (name-compare '(ZZZZ . ZZZ) SSAX:largest-unres-name)))
 (assert (eq? '> (name-compare SSAX:largest-unres-name '(ZZZZ . ZZZ) )))
)

(run-test
 (assert (equal? "p1 content "
    (call-with-input-string "<?pi1  p1 content ?>"
      (lambda (port)
	(SSAX:read-markup-token port)
	(SSAX:read-pi-body-as-string port)))))
 (assert (equal? "pi2? content? ?"
    (call-with-input-string "<?pi2 pi2? content? ??>"
      (lambda (port)
	(SSAX:read-markup-token port)
	(SSAX:read-pi-body-as-string port)))))
)

(run-test (letrec
  ((consumer (lambda (fragment foll-fragment seed)
     (cons* (if (equal? foll-fragment (string #\newline))
		(string-append " NL" nl) foll-fragment) fragment seed)))
   (test (lambda (str expected-result)
          (newline) (display "body: ") (write str) (newline) (display "Result: ")
	  (let ((result
		 (reverse 
		  (call-with-input-string str
		    (lambda (port) (SSAX:read-CDATA-body port consumer '()))
		    ))))
	    (write result)
	    (assert (equal? result expected-result)))))
   )
  (test "]]>" '())
  (test "abcd]]>" '("abcd" ""))
  (test "abcd]]]>" '("abcd" "" "]" ""))
  (test "abcd]]]]>" '("abcd" "" "]" "" "]" ""))
  (test "abcd]]]]]>" '("abcd" "" "]" "" "]" "" "]" ""))
  (test "abcd]]]a]]>" '("abcd" "" "]" "" "]]" "" "a" ""))
;; ###
;;  (test "abc\r\ndef\n]]>" '("abc" " NL\n" "def" " NL\n"))
;;  (test "\r\n\r\n]]>" '("" " NL\n" "" " NL\n"))
;;  (test "\r\n\r\na]]>" '("" " NL\n" "" " NL\n" "a" ""))
  (test "abc&!!!]]>" '("abc" "&" "" "" "!!!" ""))
  (test "abc]]&gt;&gt&amp;]]]&gt;and]]>"
    '("abc" "" "]]" "" "" ">" "" "&" "gt" "" "" "&" "amp" "" ";" "" "]" ""
      "]]" "" "" ">" "and" ""))
))

(run-test (letrec
    ((test (lambda (str decl-entities expected-res)
          (newline) (display "input: ") (write str) (newline) (display "Result: ")
	  (let ((result
		 (call-with-input-string str
              (lambda (port) (SSAX:read-attributes port decl-entities)))))
	    (write result) (newline)
	    (assert (equal? result expected-res))))))
    (test "" '() '())
    (test "html:href='ref1' html:src='ref2'" '()
	  `(((,(string->symbol "html") . ,(string->symbol "href"))
	     . "ref1")
	    ((,(string->symbol "html") . ,(string->symbol "src"))
	     . "ref2")))
    (test "html:href='ref1' xml:html='ref2'" '()
	  `(((,(string->symbol "html") . ,(string->symbol "href"))
	     . "ref1")
	    ((,SSAX:Prefix-XML . ,(string->symbol "html"))
	     . "ref2")))
    (assert (failed? (test "html:href='ref1' html:href='ref2'" '() '())))
    (assert (failed? (test "html:href='<' html:href='ref2'" '() '())))
    (assert (failed? (test "html:href='ref1' html:href='&ref2;'" '() '())))
))

(run-test
 (let* ((namespaces
	'((HTML UHTML . URN-HTML)
	  (HTML UHTML-1 . URN-HTML)
	  (A    UHTML . URN-HTML)))
	(namespaces-def
	 (cons
	  '(*DEFAULT* DEF . URN-DEF) namespaces))
	(namespaces-undef
	 (cons
	  '(*DEFAULT* #f . #f) namespaces-def))
	(port (current-input-port)))

   (assert (equal? 'ABC 
		   (SSAX:resolve-name port 'ABC namespaces #t)))
   (assert (equal? '(DEF . ABC)
		   (SSAX:resolve-name port 'ABC namespaces-def #t)))
   (assert (equal? 'ABC
		   (SSAX:resolve-name port 'ABC namespaces-def #f)))
   (assert (equal? 'ABC
		   (SSAX:resolve-name port 'ABC namespaces-undef #t)))
   (assert (equal? '(UHTML . ABC)
		   (SSAX:resolve-name port '(HTML . ABC) namespaces-def #t)))
   (assert (equal? '(UHTML . ABC)
		   (SSAX:resolve-name port '(HTML . ABC) namespaces-def #f)))
   (assert (equal? `(,SSAX:Prefix-XML . space)
		   (SSAX:resolve-name port 
		       `(,(string->symbol "xml") . space) namespaces-def #f)))
   (assert (failed?
		   (SSAX:resolve-name port '(XXX . ABC) namespaces-def #f)))
))
(run-test
 (let* ((urn-a (string->symbol "urn:a"))
	(urn-b (string->symbol "urn:b"))
	(urn-html (string->symbol "http://w3c.org/html"))
	(namespaces
	 `((#f ,(string->symbol "UHTML") . ,urn-html)
	   (,(string->symbol "A")  ,(string->symbol "UA") . ,urn-a)))
	  (test
	   (lambda (tag-head-name elems str)
	     (call-with-input-string str
		(lambda (port)
		  (call-with-values
		      (lambda ()
			      (SSAX:complete-start-tag
			       (call-with-input-string tag-head-name
				      (lambda (port) (SSAX:read-QName port)))
			       port
			       elems '() namespaces))
		    list))))))

   ; First test with no validation of elements
   ;(test "TAG1" #f "")
   (assert (equal? `(,(string->symbol "TAG1") () ,namespaces ANY)
		   (test "TAG1" #f ">")))
   (assert (equal? `(,(string->symbol "TAG1") () ,namespaces EMPTY-TAG)
		   (test "TAG1" #f "/>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "a")) ,namespaces EMPTY-TAG)
		   (test "TAG1" #f "HREF='a'/>")))
   (assert (equal? `((,(string->symbol "UA") . ,(string->symbol "TAG1")) ((,(string->symbol "HREF") . "a"))
		     ,(cons `(*DEFAULT* ,(string->symbol "UA") . ,urn-a) namespaces) ANY)
		   (test "TAG1" #f "HREF='a' xmlns='urn:a'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "a"))
		     ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
		   (test "TAG1" #f "HREF='a' xmlns=''>")))
   (assert (failed? (test "UA:TAG1" #f "HREF='a' xmlns=''/>")))
   (assert (equal? `((,(string->symbol "UA") . ,(string->symbol "TAG1")) (((,(string->symbol "UA") . ,(string->symbol "HREF")) . "a"))
		     ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
		   (test "A:TAG1" #f "A:HREF='a' xmlns=''>")))
   (assert (equal? `((,(string->symbol "UA") . ,(string->symbol "TAG1")) (((,(string->symbol "UA") . ,(string->symbol "HREF")) . "a"))
		     ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) ANY)
		   (test "A:TAG1" #f "A:HREF='a' xmlns='urn:b'>")))
   (assert (failed? (test "B:TAG1" #f "A:HREF='a' xmlns:b=''/>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) (((,(string->symbol "UA") . ,(string->symbol "HREF")) . "a"))
		     ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) ANY)
		   (test "B:TAG1" #f "A:HREF='a' xmlns:B='urn:b'>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) (((,(string->symbol "UA") . ,(string->symbol "HREF")) . "a")
					 ((,urn-b . ,(string->symbol "SRC")) . "b"))
		     ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) ANY)
		   (test "B:TAG1" #f 
			 "B:SRC='b' A:HREF='a' xmlns:B='urn:b'>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) (((,(string->symbol "UA") . ,(string->symbol "HREF")) . "a")
					 ((,urn-b . ,(string->symbol "HREF")) . "b"))
			  ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) ANY)
		   (test "B:TAG1" #f 
			 "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:b'>")))
   ; must be an error! Duplicate attr
   (assert (failed? (test "B:TAG1" #f
			  "HREF=\"b\" HREF='a' xmlns:B='urn:a'/>")))
   ; must be an error! Duplicate attr after ns expansion
   (assert (failed? (test "B:TAG1" #f 
			  "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:a'/>")))
   (assert (equal? `((,(string->symbol "UA") . ,(string->symbol "TAG1")) ((,(string->symbol "HREF") . "a")
					((,(string->symbol "UA") . ,(string->symbol "HREF")) . "b"))
		     ,(cons `(*DEFAULT* ,(string->symbol "UA") . ,urn-a) namespaces) ANY)
		   (test "TAG1" #f 
			 "A:HREF=\"b\" HREF='a' xmlns='urn:a'>")))
   (assert (equal? `(,(string->symbol "TAG1") (((,(string->symbol "UHTML") . ,(string->symbol "HREF")) . "a")
			      ((,urn-b . ,(string->symbol "HREF")) . "b"))
		     ,(append `(
			 (,(string->symbol "HTML") ,(string->symbol "UHTML") . ,urn-html)
			 (,(string->symbol "B") ,urn-b . ,urn-b))
			      namespaces) ANY)
		   (test "TAG1" #f 
			 "B:HREF=\"b\" xmlns:B='urn:b' xmlns:HTML='http://w3c.org/html' HTML:HREF='a' >")))

   ; Now test the validating parsing
   ; No decl for tag1
   (assert (failed? (test "TAG1" '((TAG2 ANY ()))
			  "B:HREF='b' xmlns:B='urn:b'>")))
   ; No decl for HREF elem
   (assert (failed?
	    (test "TAG1" `((,(string->symbol "TAG1") ANY ()))
		  "B:HREF='b' xmlns:B='urn:b'>")))

   ; No decl for HREF elem
   (assert (failed?
	    (test "TAG1" `((,(string->symbol "TAG1") ANY ((,(string->symbol "HREF1") CDATA IMPLIED #f))))
	    "B:HREF='b' xmlns:B='urn:b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces EMPTY-TAG)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f))))
	     "HREF='b'/>")))

   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f))))
	     "HREF='b'>")))


   ; Req'd attribute not given error
   (assert (failed? 
	    (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f))))
		  ">")))
   ; Wrong content-type of the attribute
   (assert (failed? 
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") ("c") REQUIRED #f))))
	     "HREF='b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") ("c" "b") IMPLIED #f))))
	     "HREF='b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA IMPLIED "c"))))
	     "HREF='b'>")))
   ; Bad fixed attribute
   (assert (failed? 
	 (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA FIXED "c"))))
	       "HREF='b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA FIXED "b"))))
	     "HREF='b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA FIXED "b")))) ">")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b")) ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA IMPLIED "b")))) ">")))
   (assert (equal? `(,(string->symbol "TAG1") () ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA IMPLIED #f)))) ">")))
   ; Undeclared attr
   (assert (failed? 
	(test "TAG1"
	      `((,(string->symbol "TAG1") PCDATA (((,(string->symbol "A") . ,(string->symbol "HREF")) CDATA IMPLIED "c"))))
	      "HREF='b'>")))
   (assert (equal? `(,(string->symbol "TAG1") ((,(string->symbol "HREF") . "b") ((,(string->symbol "UA") . ,(string->symbol "HREF")) . "c"))
			  ,namespaces PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f)
				       ((,(string->symbol "A") . ,(string->symbol "HREF")) CDATA IMPLIED "c"))))
	     "HREF='b'>")))
   (assert (equal? `((,(string->symbol "UA") . ,(string->symbol "TAG1"))
		     ((,(string->symbol "HREF") . "b") ((,(string->symbol "UA") . ,(string->symbol "HREF")) . "c"))
		     ,namespaces PCDATA)
       (test "A:TAG1" `(((,(string->symbol "A") . ,(string->symbol "TAG1")) PCDATA
			 ((,(string->symbol "HREF") NMTOKEN REQUIRED #f)
			  ((,(string->symbol "A") . ,(string->symbol "HREF")) CDATA IMPLIED "c"))))
	     "HREF='b'>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) ((,(string->symbol "HREF") . "b"))
		     ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) PCDATA)
       (test "B:TAG1" `(((,(string->symbol "B") . ,(string->symbol "TAG1")) PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f)
			   ((,(string->symbol "xmlns") . ,(string->symbol "B")) CDATA IMPLIED "urn:b"))))
	     "HREF='b'>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) (((,urn-b . ,(string->symbol "HREF")) . "b"))
			  ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) PCDATA)
       (test "B:TAG1" `(((,(string->symbol "B") . ,(string->symbol "TAG1")) PCDATA
			 (((,(string->symbol "B") . ,(string->symbol "HREF")) CDATA REQUIRED #f)
			  ((,(string->symbol "xmlns") . ,(string->symbol "B")) CDATA IMPLIED "urn:b"))))
	     "B:HREF='b'>")))
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) ((,(string->symbol "HREF") . "b"))
		     ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f)
			   (,(string->symbol "xmlns") CDATA IMPLIED "urn:b"))))
	     "HREF='b'>")))
   ; xmlns not declared
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) ((,(string->symbol "HREF") . "b"))
		     ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
       (test "TAG1" `((,(string->symbol "TAG1") PCDATA ((,(string->symbol "HREF") CDATA REQUIRED #f)
			   )))
	     "HREF='b' xmlns='urn:b'>")))
   ; xmlns:B not declared
   (assert (equal? `((,urn-b . ,(string->symbol "TAG1")) (((,urn-b . ,(string->symbol "HREF")) . "b"))
		     ,(cons `(,(string->symbol "B") ,urn-b . ,urn-b) namespaces) PCDATA)
       (test "B:TAG1" `(((,(string->symbol "B") . ,(string->symbol "TAG1")) PCDATA
			 (((,(string->symbol "B") . ,(string->symbol "HREF")) CDATA REQUIRED #f)
			   )))
	     "B:HREF='b' xmlns:B='urn:b'>")))
))

(run-test (letrec
  ((a-tag (make-xml-token 'START (string->symbol "BR")))
   (a-ref (make-xml-token 'ENTITY-REF (string->symbol "lt")))
   (eof-object (with-input-from-string "" read))
   (str-handler (lambda (fragment foll-fragment seed)
     (if (string-null? foll-fragment) (cons fragment seed)
	 (cons* foll-fragment fragment seed))))
   (test (lambda (str expect-eof? expected-data expected-token)
	   (newline) (display "body: ") (write str) (newline) (display "Result: ")
	  (let*-values
	   (((seed token)
	     (call-with-input-string str
		(lambda (port)
		 (SSAX:read-char-data port expect-eof? str-handler '()))))
	    ((result) (reverse seed)))
	   (write result)
	   (display " ")
	   (display token)
	   (assert (equal? result expected-data)
		   (equal? token expected-token)))))
   )
  (test "" #t '() eof-object)
  (assert (failed? (test "" #f '() eof-object)))
  (test "  " #t '("  ") eof-object)
  (test "<BR/>" #f '() a-tag)
  (test " <BR  />" #f '(" ") a-tag)

  (test " &lt;" #f '(" ") a-ref)
  (test " a&lt;" #f '(" a") a-ref)
  (test " a &lt;" #f '(" a ") a-ref)

  (test " <!-- comment--> a  a<BR/>" #f '(" " " a  a") a-tag)
;;   (test " <!-- comment-->\ra  a<BR/>" #f '(" " "" "\n" "a  a") a-tag)
;;   (test " <!-- comment-->\r\na  a<BR/>" #f '(" " "" "\n" "a  a") a-tag)
;;   (test " <!-- comment-->\r\na\t\r\r\na<BR/>" #f
;; 	'(" " "" "\n" "a\t" "\n" "" "\n" "a") a-tag)
  (test "a<!-- comment--> a  a<BR/>" #f '("a" " a  a") a-tag)
  (test "&#x21;<BR/>" #f '("" "!") a-tag)
;;   (test "&#x21;\n<BR/>" #f '("" "!" "\n") a-tag)
;;   (test "\t&#x21;\n<BR/>" #f '("\t" "!" "\n") a-tag)
;;   (test "\t&#x21;\na a<BR/>" #f '("\t" "!" "\na a") a-tag)
;;   (test "\t&#x21;\ra a<BR/>" #f '("\t" "!" "" "\n" "a a") a-tag)

;;   (test " \ta &#x21;   b <BR/>" #f '(" \ta " "!" "   b ") a-tag)
;;   (test " \ta &#x20;   b <BR/>" #f '(" \ta " " " "   b ") a-tag)

  (test "<![CDATA[<]]><BR/>" #f '("<") a-tag)
  (test "<![CDATA[]]]><BR/>" #f '("]") a-tag)
;;   (test "\t<![CDATA[<]]><BR/>" #f '("\t" "<") a-tag)
;;   (test "\t<![CDATA[<]]>a b<BR/>" #f '("\t" "<" "a b") a-tag)
;;   (test "\t<![CDATA[<]]>  a b<BR/>" #f '("\t" "<" "  a b") a-tag)

;;  (test "\td <![CDATA[  <\r\r\n]]>  a b<BR/>" #f 
;;	'("\td " "  <" "\n" "" "\n" "  a b") a-tag)
))

(run-test
 (pp (SSAX:make-pi-parser '()))
 (pp (SSAX:make-pi-parser `((xml . ,(lambda (port target seed) seed)))))
 (pp (SSAX:make-pi-parser `((xml . ,(lambda (port target seed) seed))
			    (html . ,list)
			    (*DEFAULT* . ,SSAX:warn))))
)

(run-test
 (letrec ((simple-parser
	   (lambda (str doctype-fn)
	     (call-with-input-string str
		 (lambda (port)
		   ((SSAX:make-parser
		     'NEW-LEVEL-SEED 
		     (lambda (elem-gi attributes namespaces
				      expected-content seed)
		       '())
   
		     'FINISH-ELEMENT
		     (lambda (elem-gi attributes namespaces parent-seed seed)
		       (let
			   ((seed (if (null? namespaces) (reverse seed)
				      (cons (list '*NAMESPACES* namespaces)
					    (reverse seed)))))
			 (let ((seed (if (attlist-null? attributes) seed
					 (cons 
					  (cons '@ 
					   (map (lambda (attr)
					      (list (car attr) (cdr attr)))
						(attlist->alist attributes)))
					  seed))))
			   (cons (cons elem-gi seed) parent-seed))))

		     'CHAR-DATA-HANDLER
		     (lambda (string1 string2 seed)
		       (if (string-null? string2) (cons string1 seed)
			   (cons* string2 string1 seed)))

		     'DOCTYPE
		     (lambda (port docname systemid internal-subset? seed)
		       (when internal-subset?
			  (SSAX:warn port
			    "Internal DTD subset is not currently handled ")
			  (SSAX:skip-internal-dtd port))
		       (SSAX:warn port "DOCTYPE DECL " docname " "
			     systemid " found and skipped")
		       (doctype-fn docname seed))

		     'UNDECL-ROOT
		     (lambda (elem-gi seed)
		       (doctype-fn elem-gi seed))
		     )
		    port '())))))

	  (dummy-doctype-fn (lambda (elem-gi seed) (values #f '() '() seed)))
	  (test
	   (lambda (str doctype-fn expected)
	     (cout nl "Parsing: " str nl)
	     (let ((result (simple-parser str doctype-fn)))
	       (write result)
	       (assert (equal? result expected)))))
	  )

   (test "<BR/>" dummy-doctype-fn `((,(string->symbol "BR"))))
   (assert (failed? (test "<BR>" dummy-doctype-fn '())))
   (test "<BR></BR>" dummy-doctype-fn `((,(string->symbol "BR"))))
   (assert (failed? (test "<BR></BB>" dummy-doctype-fn '())))

   (test "   <A HREF='URL'> link <I>itlink </I> &amp;amp;</A>"
	 dummy-doctype-fn 
	 `((,(string->symbol "A") (@ (,(string->symbol "HREF") "URL")) " link " (,(string->symbol "I") "itlink ")
	    " " "&" "amp;")))

   (test
      "   <A HREF='URL' xml:space='preserve'> link <I>itlink </I> &amp;amp;</A>" dummy-doctype-fn 
      `((,(string->symbol "A") (@ (,(string->symbol "HREF") "URL") ((,(string->symbol "xml") . ,(string->symbol "space")) "preserve"))
	   " link " (,(string->symbol "I") "itlink ") " " "&" "amp;")))

   (test "   <A HREF='URL' xml:space='preserve'> link <I xml:space='default'>itlink </I> &amp;amp;</A>" dummy-doctype-fn
	 `((,(string->symbol "A") (@ (,(string->symbol "HREF") "URL") ((,(string->symbol "xml") . ,(string->symbol "space")) "preserve"))
	      " link "
	      (,(string->symbol "I") (@ ((,(string->symbol "xml") . ,(string->symbol "space")) "default")) "itlink ")
	      " " "&" "amp;")))
;;    (test "<itemize><item>This   is item 1 </item>\n<!-- Just:a comment --><item>Item 2</item>\n </itemize>" dummy-doctype-fn 
;; 	 '((,(string->symbol "itemize") (,(string->symbol "item") "This   is item 1 ")
;; 	    "\n" (,(string->symbol "item") "Item 2") "\n ")))
;;  (test " <P><![CDATA[<BR>\n<![CDATA[<BR>]]&gt;]]></P>"
;;	dummy-doctype-fn '((,(string->symbol "P") "<BR>" "\n" "<![CDATA[<BR>" "]]" "" ">")))

  (test (string-append "<?xml version='1.0'?>" nl nl "<Reports TStamp='1'></Reports>")
	dummy-doctype-fn `((,(string->symbol "Reports") (@ (,(string->symbol "TStamp") "1")))))
  (test (string-append nl "<?PI xxx?><!-- Comment " nl " -" nl " -->" nl "<?PI1 zzz?><T/>")
	dummy-doctype-fn `((,(string->symbol "T"))))
  (test (string-append "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->" nl "<T/>")
	(lambda (elem-gi seed) (assert (equal? elem-gi (string->symbol "T")))
		(values #f '() '() seed))
	`((,(string->symbol "T"))))
  (test (string-append "<!DOCTYPE T PUBLIC '//EN/T' \"system1\" [ <!ELEMENT a 'aa'> ]>" nl "<?pi?><T/>")
	(lambda (elem-gi seed) (assert (equal? elem-gi (string->symbol "T")))
		(values #f '() '() seed))
	`((,(string->symbol "T"))))
  (test "<BR/>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") EMPTY ())) '() '() seed)) `((,(string->symbol "BR"))))
  (test "<BR></BR>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") EMPTY ())) '() '() seed)) `((,(string->symbol "BR"))))
  (assert (failed? (test "<BR>aa</BR>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") EMPTY ())) '() '() seed)) '())))
  (test "<BR>aa</BR>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") PCDATA ())) '() '() seed)) `((,(string->symbol "BR") "aa")))
  (assert (failed? (test "<BR>a<I>a</I></BR>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") PCDATA ())) '() '() seed)) '())))
  (test "<BR>a<I>a</I></BR>"
	(lambda (elem-gi seed)
	  (values `((,(string->symbol "BR") ANY ()) (,(string->symbol "I") PCDATA ())) '() '() seed))
	  `((,(string->symbol "BR") "a" (,(string->symbol "I") "a"))))


  (test "<DIV>Example: \"&example;\"</DIV>"
	(lambda (elem-gi seed)
	  (values #f '((example . "<P>An    ampersand (&#38;) may   be escaped numerically (&#38;#38;) or with a general entity (&amp;amp;).</P>")) '() seed))
	`((,(string->symbol "DIV") "Example: \""
	   (,(string->symbol "P") "An    ampersand (" "&" ") may   be escaped numerically (" "&" "#38;) or with a general entity (" "&" "amp;).") "\"")))
 (test "<DIV>Example: \"&example;\" <P/></DIV>"
	(lambda (elem-gi seed)
	  (values #f `((,(string->symbol "quote") . "<I>example:</I> ex")
		       (,(string->symbol "example") . "<Q>&quote;!</Q>?")) '() seed))
	  `((,(string->symbol "DIV") "Example: \"" (,(string->symbol "Q") (,(string->symbol "I") "example:") " ex" "!") "?"
		 "\" "  (,(string->symbol "P")))))
 (assert (failed?
   (test "<DIV>Example: \"&example;\" <P/></DIV>"
	(lambda (elem-gi seed)
	  (values #f `((,(string->symbol "quote") . "<I>example:")
		       (,(string->symbol "example") . "<Q>&quote;</I>!</Q>?")) '() seed))
	'())
	  `((,(string->symbol "DIV") "Example: \"" (,(string->symbol "Q") (,(string->symbol "I") "example:") "ex" "!") "?"
		 "\""  (,(string->symbol "P"))))))

 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values #f '() '() seed))
       `(((,(string->symbol "URI1") . ,(string->symbol "DIV")) (@ (,(string->symbol "B") "B") ((,(string->symbol "URI1") . ,(string->symbol "B")) "A"))
	  (*NAMESPACES* ((,(string->symbol "A") ,(string->symbol "URI1") . ,(string->symbol "URI1"))
			 (*DEFAULT* ,(string->symbol "URI1") . ,(string->symbol "URI1"))))
	  ((,(string->symbol "URI1") . ,(string->symbol "P"))
	   (*NAMESPACES* ((*DEFAULT* #f . #f) (,(string->symbol "A") ,(string->symbol "URI1") . ,(string->symbol "URI1"))
			  (*DEFAULT* ,(string->symbol "URI1") . ,(string->symbol "URI1"))))
	   (,(string->symbol "BR")
	    (*NAMESPACES* ((*DEFAULT* #f . #f)
			   (,(string->symbol "A") ,(string->symbol "URI1") . ,(string->symbol "URI1"))
			   (*DEFAULT* ,(string->symbol "URI1") . ,(string->symbol "URI1")))))))))
 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values #f '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
       `(((,(string->symbol "UA") . ,(string->symbol "DIV")) (@ (,(string->symbol "B") "B") ((,(string->symbol "UA") . ,(string->symbol "B")) "A"))
	  (*NAMESPACES* ((,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			 (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	  ((,(string->symbol "UA") . ,(string->symbol "P"))
	   (*NAMESPACES* ((*DEFAULT* #f . #f) (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			  (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	   (,(string->symbol "BR")
	    (*NAMESPACES* ((*DEFAULT* #f . #f) (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			   (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			   (#f ,(string->symbol "UA") . ,(string->symbol "URI1")))))))))
 ; uniqattr should fail
 (assert (failed?
 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values 
	   `((,(string->symbol "DIV") ANY ((,(string->symbol "B") CDATA IMPLIED #f)
		       ((,(string->symbol "A") . ,(string->symbol "B")) CDATA IMPLIED #f)
		       ((,(string->symbol "C") . ,(string->symbol "B")) CDATA IMPLIED "xx")
		       ((,(string->symbol "xmlns") . ,(string->symbol "C")) CDATA IMPLIED "URI1")
		       ))
	     ((,(string->symbol "A") . ,(string->symbol "P")) ANY ()) (,(string->symbol "BR") ,(string->symbol "EMPTY") ()))
	   '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
	'())))
 ; prefix C undeclared
 (assert (failed?
 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values 
	   `((,(string->symbol "DIV") ANY ((,(string->symbol "B") CDATA IMPLIED #f)
		       (,(string->symbol "xmlns")  CDATA IMPLIED "URI1")
		       ((,(string->symbol "A") . ,(string->symbol "B")) CDATA IMPLIED #f)
		       ((,(string->symbol "C") . ,(string->symbol "B")) CDATA IMPLIED "xx")
		       ))
	     ((,(string->symbol "A") . ,(string->symbol "P")) ANY ()) (,(string->symbol "BR") EMPTY ()))
	   '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
	'())))

 ; contradiction to xmlns declaration
 (assert (failed?
 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values 
	   `((,(string->symbol "DIV") ANY ((,(string->symbol "B") CDATA IMPLIED #f)
		       (,(string->symbol "xmlns")  CDATA FIXED "URI2")
		       ((,(string->symbol "A") . ,(string->symbol "B")) CDATA IMPLIED #f)
		       ))
	     ((,(string->symbol "A") . ,(string->symbol "P")) ANY ()) (,(string->symbol "BR") EMPTY ()))
	   '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
	'())))

 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values 
	   `((,(string->symbol "DIV") ANY ((,(string->symbol "B") CDATA IMPLIED #f)
		       (,(string->symbol "xmlns")  CDATA FIXED "URI1")
		       ((,(string->symbol "A") . ,(string->symbol "B")) CDATA IMPLIED #f)
		       ))
	     ((,(string->symbol "A") . ,(string->symbol "P")) ANY ()) (,(string->symbol "BR") EMPTY ()))
	   '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
       `(((,(string->symbol "UA") . ,(string->symbol "DIV")) (@ (,(string->symbol "B") "B") ((,(string->symbol "UA") . ,(string->symbol "B")) "A"))
	  (*NAMESPACES* ((*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			 (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	  ((,(string->symbol "UA") . ,(string->symbol "P"))
	   (*NAMESPACES* ((*DEFAULT* #f . #f) 
			  (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			  (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	   (,(string->symbol "BR")
	    (*NAMESPACES* ((*DEFAULT* #f . #f) (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			   (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1")))))))))

 (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
	(lambda (elem-gi seed)
	  (values 
	   `((,(string->symbol "DIV") ANY ((,(string->symbol "B") CDATA IMPLIED #f)
			  ((,(string->symbol "A") . ,(string->symbol "B")) CDATA IMPLIED #f)
			  ((,(string->symbol "C") . ,(string->symbol "B")) CDATA IMPLIED "xx")
			  ((,(string->symbol "xmlns") . ,(string->symbol "C")) CDATA IMPLIED "URI2")
		       ))
	     ((,(string->symbol "A") . ,(string->symbol "P")) ANY ()) (,(string->symbol "BR") EMPTY ()))
	   '() `((#f ,(string->symbol "UA") . ,(string->symbol "URI1"))) seed))
	`(((,(string->symbol "UA") . ,(string->symbol "DIV")) (@ (,(string->symbol "B") "B") ((,(string->symbol "UA") . ,(string->symbol "B")) "A")
			       ((,(string->symbol "URI2") . ,(string->symbol "B")) "xx"))
	   (*NAMESPACES* ((*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			  (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			  (,(string->symbol "C") ,(string->symbol "URI2") . ,(string->symbol "URI2"))
			  (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	   ((,(string->symbol "UA") . ,(string->symbol "P"))
	    (*NAMESPACES* ((*DEFAULT* #f . #f) (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			   (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			   (,(string->symbol "C") ,(string->symbol "URI2") . ,(string->symbol "URI2")) (#f ,(string->symbol "UA") . ,(string->symbol "URI1"))))
	    (,(string->symbol "BR") 
	     (*NAMESPACES* ((*DEFAULT* #f . #f)
			    (*DEFAULT* ,(string->symbol "UA") . ,(string->symbol "URI1"))
			    (,(string->symbol "A") ,(string->symbol "UA") . ,(string->symbol "URI1"))
			    (,(string->symbol "C") ,(string->symbol "URI2") . ,(string->symbol "URI2"))
			    (#f ,(string->symbol "UA") . ,(string->symbol "URI1")))))))))
))

   
(run-test (letrec
    ((test (lambda (str namespace-assig expected-res)
	  (newline) (display "input: ")
	  (write str) (newline) (display "Result: ")
	  (let ((result
		 (call-with-input-string str
		     (lambda (port)
		       (SSAX:XML->SXML port namespace-assig)))))
	    (pp result)
	    (assert (equal_? result expected-res))))))

    (test " <BR/>" '() '(*TOP* (BR)))
    (test "<BR></BR>" '() '(*TOP* (BR)))
    (test (string-append " <BR CLEAR='ALL' " nl "CLASS='Class1'/>") '()
	  '(*TOP* (BR (@ (CLEAR "ALL") (CLASS "Class1")))))
    (test "   <A HREF='URL'>  link <I>itlink </I> &amp;amp;</A>" '()
	  '(*TOP* (A (@ (HREF "URL")) "  link " (I "itlink ") " &amp;")))
    (test "   <A HREF='URL' xml:space='preserve'>  link <I>itlink </I> &amp;amp;</A>" '()
	  '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
		     "  link " (I "itlink ") " &amp;")))
    (test "   <A HREF='URL' xml:space='preserve'>  link <I xml:space='default'>itlink </I> &amp;amp;</A>" '()
	  '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
		     "  link " (I (@ (xml:space "default"))
				  "itlink ") " &amp;")))
    (test " <P><?pi1  p1 content ?>?<?pi2 pi2? content? ??></P>" '()
	  '(*TOP* (P (*PI* pi1 "p1 content ") "?"
		     (*PI* pi2 "pi2? content? ?"))))
;;    (test (string-append " <P>some text <![CDATA[<]]>1" nl "&quot;<B>strong</B>&quot;" nl "</P>")
;;	  '()
;;	  '(*TOP* (P "some text <1\n\"" (B "strong") "\"\n")))
;;    (test " <P><![CDATA[<BR>\n<![CDATA[<BR>]]&gt;]]></P>" '()
;;	  '(*TOP* (P "<BR>\n<![CDATA[<BR>]]>")))
;    (test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
;	  '(*TOP* (T1 (T2 "it's\nand   that\n") "\n\n\n")))
;;    (test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
;;	  '(*TOP* (T1 (T2 "it's\nand   that\n"))))
;;    (test "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->\n<T/>" '()
;;	  '(*TOP* (T)))
;;    (test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
;;	  '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
;;                (NET (@ (certified "certified")) " 67 ")
;;                (GROSS " 95 "))
;;		  ))
;     (test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
; 	  '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
;                "\n" (NET (@ (certified "certified")) " 67 ")
;                "\n" (GROSS " 95 ") "\n")
; 		  ))
    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '()
	  '(*TOP* (URI1:DIV (@ (URI1:B "A") (B "B")) (URI1:P (BR)))))
    (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '((UA . "URI1"))
	  '(*TOP* (*NAMESPACES* (UA "URI1"))
		  (UA:DIV (@ (UA:B "A") (B "B")) (UA:P (BR)))))

    ; A few tests from XML Namespaces Recommendation
    (test (string-append
	   "<x xmlns:edi='http://ecommerce.org/schema'>"
           "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
           "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
	   nl
           "</x>") '()
	   '(*TOP* 
	     (x (lineItem
		 (@ (http://ecommerce.org/schema:taxClass "exempt"))
            "Baby food"))))
    (test (string-append 
	   "<x xmlns:edi='http://ecommerce.org/schema'>"
           "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
           "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
           "</x>") '((EDI . "http://ecommerce.org/schema"))
	   '(*TOP*
	     (*NAMESPACES* (EDI "http://ecommerce.org/schema"))
	     (x (lineItem
		 (@ (EDI:taxClass "exempt"))
            "Baby food"))))

    (test (string-append
	   "<bk:book xmlns:bk='urn:loc.gov:books' "
                     "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	   "<bk:title>Cheaper by the Dozen</bk:title>"
           "<isbn:number>1568491379</isbn:number></bk:book>")
	  '()
	  '(*TOP* (urn:loc.gov:books:book
		   (urn:loc.gov:books:title "Cheaper by the Dozen")
		   (urn:ISBN:0-395-36341-6:number "1568491379"))))

    (test (string-append
	   "<!-- initially, the default namespace is 'books' -->"
           "<book xmlns='urn:loc.gov:books' "
           "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
           "<title>Cheaper by the Dozen</title>"
           "<isbn:number>1568491379</isbn:number>"
	   "<notes>"
	   "<!-- make HTML the default namespace for some commentary -->"
	   "<p xmlns='urn:w3-org-ns:HTML'>"
	   "This is a <i>funny</i> book!"
            "</p>"
            "</notes>"
            "</book>") '()
	    '(*TOP* (urn:loc.gov:books:book
		   (urn:loc.gov:books:title "Cheaper by the Dozen")
		   (urn:ISBN:0-395-36341-6:number "1568491379")
		   (urn:loc.gov:books:notes
		    (urn:w3-org-ns:HTML:p 
		     "This is a " (urn:w3-org-ns:HTML:i "funny")
		     " book!")))))

    (test (string-append
	   "<Beers>"
           "<!-- the default namespace is now that of HTML -->"
           "<table xmlns='http://www.w3.org/TR/REC-html40'>"
           "<th><td>Name</td><td>Origin</td><td>Description</td></th>"
           "<tr>"
           "<!-- no default namespace inside table cells -->"
           "<td><brandName xmlns=\"\">Huntsman</brandName></td>"
           "<td><origin xmlns=''>Bath, UK</origin></td>"
           "<td>"
              "<details xmlns=''><class>Bitter</class><hop>Fuggles</hop>"
              "<pro>Wonderful hop, light alcohol, good summer beer</pro>"
              "<con>Fragile; excessive variance pub to pub</con>"
              "</details>"
	   "</td>"
           "</tr>"
           "</table>"
           "</Beers>")
	      '((html . "http://www.w3.org/TR/REC-html40"))
	      '(*TOP*
		(*NAMESPACES* (html "http://www.w3.org/TR/REC-html40"))
		(Beers (html:table
                (html:th (html:td "Name")
                         (html:td "Origin")
                         (html:td "Description"))
                (html:tr (html:td (brandName "Huntsman"))
                         (html:td (origin "Bath, UK"))
                         (html:td 
			  (details 
			   (class "Bitter")
			(hop "Fuggles")
			(pro "Wonderful hop, light alcohol, good summer beer")
			(con "Fragile; excessive variance pub to pub"))))))))

    (test (string-append
       "<!-- 1 --><RESERVATION xmlns:HTML='http://www.w3.org/TR/REC-html40'>"
       "<!-- 2 --><NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
       "<!-- 3 --><SEAT CLASS='Y' HTML:CLASS=\"largeMonotype\">33B</SEAT>"
       "<!-- 4 --><HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>"
       "<!-- 5 --><DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
	  '((HTML . "http://www.w3.org/TR/REC-html40"))
	  '(*TOP* (*NAMESPACES* (HTML "http://www.w3.org/TR/REC-html40"))
	     (RESERVATION
	      (NAME (@ (HTML:CLASS "largeSansSerif")) "Layman, A")
	      (SEAT (@ (HTML:CLASS "largeMonotype") (CLASS "Y")) "33B")
	      (HTML:A (@ (HREF "/cgi-bin/ResStatus")) "Check Status")
	      (DEPARTURE "1997-05-24T07:55:00+1"))))

    ; Part of RDF from the XML Infoset
        (test (apply string-append (list-intersperse '(
  "<?xml version='1.0' encoding='utf-8' standalone='yes'?>"
  "<!-- this can be decoded as US-ASCII or iso-8859-1 as well,"
     "  since it contains no characters outside the US-ASCII repertoire -->"
   "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'"
   "         xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'"
   "          xmlns='http://www.w3.org/2001/02/infoset#'>"
   "<rdfs:Class ID='Boolean'/>"
   "<Boolean ID='Boolean.true'/>"
   "<Boolean ID='Boolean.false'/>"
   "<!--Info item classes-->"
   "<rdfs:Class ID='InfoItem'/>"
   "<rdfs:Class ID='Document' rdfs:subClassOf='#InfoItem'/>"
   "<rdfs:Class ID='Element' rdfs:subClassOf='#InfoItem'/>"
   "<rdfs:Class ID='Attribute' rdfs:subClassOf='#InfoItem'/>"
   "<rdfs:Class ID='InfoItemSet'
      rdfs:subClassOf='http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag'/>"
   "<rdfs:Class ID='AttributeSet' rdfs:subClassOf='#InfoItemSet'/>"
   "<!--Info item properties-->"
   "<rdfs:Property ID='allDeclarationsProcessed'>"
   "<rdfs:domain resource='#Document'/>"
   "<rdfs:range resource='#Boolean'/></rdfs:Property>"
   "<rdfs:Property ID='attributes'>"
   "<rdfs:domain resource='#Element'/>"
   "<rdfs:range resource='#AttributeSet'/>"
   "</rdfs:Property>"
   "</rdf:RDF>")
   (string #\newline)))
   '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
     (RDFS . "http://www.w3.org/2000/01/rdf-schema#")
     (ISET . "http://www.w3.org/2001/02/infoset#"))
   '(*TOP* (*NAMESPACES*
         (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
         (RDFS "http://www.w3.org/2000/01/rdf-schema#")
         (ISET "http://www.w3.org/2001/02/infoset#"))
       (*PI* xml "version='1.0' encoding='utf-8' standalone='yes'")
       (RDF:RDF
	(RDFS:Class (@ (ID "Boolean")))
	(ISET:Boolean (@ (ID "Boolean.true")))
	(ISET:Boolean (@ (ID "Boolean.false")))
	(RDFS:Class (@ (ID "InfoItem")))
	(RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Document")))
	(RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Element")))
	(RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Attribute")))
	(RDFS:Class
	 (@ (RDFS:subClassOf
	     "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
	    (ID "InfoItemSet")))
	(RDFS:Class
	 (@ (RDFS:subClassOf "#InfoItemSet") (ID "AttributeSet")))
	(RDFS:Property
	 (@ (ID "allDeclarationsProcessed"))
	 (RDFS:domain (@ (resource "#Document")))
	 (RDFS:range (@ (resource "#Boolean"))))
	(RDFS:Property
	 (@ (ID "attributes"))
	 (RDFS:domain (@ (resource "#Element")))
	 (RDFS:range (@ (resource "#AttributeSet")))))))
	  
    ; Part of RDF from RSS of the Daemon News Mall
        (test (apply string-append (list-intersperse '(
  "<?xml version='1.0'?><rdf:RDF "
    "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' "
     "xmlns='http://my.netscape.com/rdf/simple/0.9/'>"
     "<channel>"
     "<title>Daemon News Mall</title>"
     "<link>http://mall.daemonnews.org/</link>"
     "<description>Central source for all your BSD needs</description>"
     "</channel>"
     "<item>"
     "<title>Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95</title>"
     "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=880</link>"
     "</item>"
     "<item>"
     "<title>The Design and Implementation of the 4.4BSD Operating System $54.95</title>"
     "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=912&amp;category_id=1761</link>"
     "</item>"
     "</rdf:RDF>")
   (string #\newline)
   ))
   '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
     (RSS . "http://my.netscape.com/rdf/simple/0.9/")
     (ISET . "http://www.w3.org/2001/02/infoset#"))
   '(*TOP* (*NAMESPACES*
         (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
         (RSS "http://my.netscape.com/rdf/simple/0.9/")
         (ISET "http://www.w3.org/2001/02/infoset#"))
       (*PI* xml "version='1.0'")
       (RDF:RDF (RSS:channel
                  (RSS:title "Daemon News Mall")
                  (RSS:link "http://mall.daemonnews.org/")
                  (RSS:description "Central source for all your BSD needs"))
                (RSS:item
                  (RSS:title
                    "Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95")
                  (RSS:link
                    "http://mall.daemonnews.org/?page=shop/flypage&product_id=880"))
                (RSS:item
                  (RSS:title
                    "The Design and Implementation of the 4.4BSD Operating System $54.95")
                  (RSS:link
                    "http://mall.daemonnews.org/?page=shop/flypage&product_id=912&category_id=1761")))))

    (test (apply string-append (list-intersperse 
       '("<Forecasts TStamp='958082142'>"
	 "<TAF TStamp='958066200' LatLon='36.583, -121.850' BId='724915'"
	 "  SName='KMRY, MONTEREY PENINSULA'>"
	 "<VALID TRange='958068000, 958154400'>111730Z 111818</VALID>"
	 "<PERIOD TRange='958068000, 958078800'>"
	 "<PREVAILING>31010KT P6SM FEW030</PREVAILING>"
	 "</PERIOD>"
	 "<PERIOD TRange='958078800, 958104000' Title='FM2100'>"
	 "<PREVAILING>29016KT P6SM FEW040</PREVAILING>"
	 "</PERIOD>"
	 "<PERIOD TRange='958104000, 958154400' Title='FM0400'>"
	 "<PREVAILING>29010KT P6SM SCT200</PREVAILING>"
	 "<VAR Title='BECMG 0708' TRange='958114800, 958118400'>VRB05KT</VAR>"
	 "</PERIOD></TAF>"
	 "</Forecasts>")
       (string #\newline)
       ))
	  '()
	  '(*TOP* (Forecasts
		   (@ (TStamp "958082142"))
		   (TAF (@ (TStamp "958066200")
			   (SName "KMRY, MONTEREY PENINSULA")
			   (LatLon "36.583, -121.850")
			   (BId "724915"))
              (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
              (PERIOD (@ (TRange "958068000, 958078800"))
                      (PREVAILING "31010KT P6SM FEW030"))
              (PERIOD (@ (Title "FM2100") (TRange "958078800, 958104000"))
                      (PREVAILING "29016KT P6SM FEW040"))
              (PERIOD (@ (Title "FM0400") (TRange "958104000, 958154400"))
                      (PREVAILING "29010KT P6SM SCT200")
                      (VAR (@ (Title "BECMG 0708")
                              (TRange "958114800, 958118400"))
                           "VRB05KT"))))))
))

(run-test
 (newline)
 (display "All tests passed")
 (newline)
)