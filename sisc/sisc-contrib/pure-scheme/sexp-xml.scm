;; Utility module -- SAX->sexp using the Java XML parser
;;
;; Exports the following procedures.
;;
;; SEXP-XML:XML->SEXP/FILE filename -> list
;; Given a file name, parse it and return a SSAX-style sexp representing it. 
;;
;; SEXP-XML:XML->SEXP/STRING string -> list
;; Parse the XML in the given string.
;;
;; SEXP-XML:XML->SEXP/READER java-reader -> list
;; Parse XML read from the given object, which implements the Java Reaser
;; interface.
;;
;; SEXP-XML:SKIP-WHITESPACE? boolean -> boolean
;; Set the behaviour of the conversion to the sexp.  If the boolean is set
;; true, then all-whitespace content is skipped (so that
;; "<doc> <p>content  </p> </doc>" would be parsed as (doc (p "content  ")),
;; but if it is false, it is all included verbatim, so that the above would
;; parse to (doc " " (p "content  ") " ").  This is a SRFI-39 parameter, 
;; so may be set inside a PARAMETERIZE special form.
;;
;; SEXP-XML:LOCALNAMES-INTO-SYMBOLS? boolean -> boolean
;; If true (the default), element and attribute names are reported as symbols
;; (case-insensitively); if false, as strings.  This is a SRFI-39
;; parameter.
;;
;; SEXP-XML:SEXP->XML list -> string
;;
;;     (sexp-xml:sexp->xml <sexp>) => string
;;     (sexp-xml:sexp->xml <sexp> [<block-element-list> [<para-element-list>]])
;;        => string
;;
;; Takes two optional arguments: the first specifies a list of
;; elements which are to be formatted (ie, have linebreaks inserted)
;; as `block' elements (like <div> in HTML), and the second a list
;; which should be formatted as `para' elements (like HTML <p>).
;; Either may be given as 'ALL to format all like this.  The default
;; in both cases is appropriate for HTML.
;;
;; Examples:
;;
;;     (sexp-xml:sexp->xml '(top (el1 "hello") (el2 (@ (att val)))))
;;        => "<top><el1>hello</el1><el2 att='val'/></top>"
;;
;;     (sexp-xml:sexp->xml '(top (el1 "hello") (el2 (@ (att val)))) '(el1))
;;        => "<top><el1>~%hello</el1>~%~%<el2 att='val'/></top>"
;;
;;     (sexp-xml:sexp->xml '(html (head (title "SEXPs"))
;;                       (body (p (@ (class simple)) hello))))
;;        => "<html>~%<head>~%<title>SEXPs</title>~% ..."
;;
;; SEXP-XML:ESCAPE-STRING-FOR-XML string -> string
;;
;; Given a string, return the string with < and & characters escaped.
;;
;;
;; Copyright 2006 Norman Gray, <norman@astro.gla.ac.uk>
;; Released under the terms of the GNU General Public Licence.


(import s2j)
(import string-io)                      ;for with-output-to-string

(require-library 'sisc/libs/srfi/srfi-39) ;parameter objects

(module sexp-xml
    (sexp-xml:xml->sexp/file
     sexp-xml:xml->sexp/string
     sexp-xml:xml->sexp/reader
     sexp-xml:skip-whitespace?
     sexp-xml:localnames-into-symbols?
     sexp-xml:sexp->xml
     sexp-xml:escape-string-for-xml)
  
(import srfi-39)

(define-java-classes
  <org.xml.sax.content-handler>)

;; Procedure NULL-METHOD does nothing
(define (null-method name) #f)

;; Make a Qname from a (java string) NAMESPACE and LOCALNAME.
;; If NAMESPACE is an empty string, then return LOCALNAME (as a scheme string);
;; otherwise, return ("NAMESPACE" . <localname>), where "NAMESPACE" is a
;; scheme string, and <localname> is either a symbol or a scheme string,
;; depending on the value of (sexp-xml:localnames-into-symbols?).
(define (qname namespace localname)
  (define-generic-java-method
    to-lower-case)
  (let ((ns (->string namespace))
        (ln (if (sexp-xml:localnames-into-symbols?)
                (string->symbol (->string (to-lower-case localname)))
                (->string localname))))
    (if (> (string-length ns) 0)
        (cons ns ln)
        ln)))

;; Create a ssax-accumulator.  This returns a function which is carried about
;; by the SAX ContentHandler below.
(define (new-ssax-accumulator)

  (let ((stack '((*TOP*)))
        (currently-parsing? #f))
    (lambda (op . args)                    ;op a symbol, args list of strings
      (cond ((eq? op 'start-element)
             ;; args: ns (jstring), name (jstring), attlist (SAX Attributes)
             (let ((ns (car args))
                   (el (cadr args))
                   (attlist (get-sax-atts-list (caddr args)))) ;attlist
               (if attlist
                   (set! stack
                         (cons `((@ ,@attlist)
                                 ,(qname ns el))
                               stack))
                   (set! stack
                         (cons (list (qname ns el))
                               stack)))))
            ((eq? op 'end-element)
             (set! stack `((,(reverse (car stack)) . ,(cadr stack))
                           . ,(cddr stack))))
            ((eq? op 'characters)
             ;; append characters to the top of the stack.  The 'normal' case
             ;; is to just  cons the new string to the list that is the car
             ;; of the stack.  If, however, the car of that list is a string,
             ;; and it's a 2+ element list (so that the string is not the
             ;; element name in localnames-into-symbols mode), then
             ;; string-append the new string onto the end of the existing one.
             (let ((newstring (car args)))
               (if (and (string? (caar stack))
                        (> (length (car stack)) 1))
                   (set-car! (car stack) (string-append (caar stack) newstring))
                   (set-car! stack
                             (cons newstring (car stack))))))
            ((eq? op 'processing-instruction)
             (set-car! stack
                       `((*PI* ,(car args) ,(cadr args)) . ,(car stack))))
            ((eq? op 'start-document)
             (set! currently-parsing? #t))
            ((eq? op 'end-document)
             (set! currently-parsing? #f))
            ((eq? op '*TOP*)
             (if currently-parsing?
                 (error 'new-ssax-accumulator "Can't retrieve *TOP* mid-parse")
                 (reverse (car stack))))
            (else
             (error 'new-ssax-accumulator "Unknown op: ~a" op))))))

                                        ;Given a SAX Attributes object, return
                                        ;the list of attributes as a list of
                                        ;(symbol string) lists.  If there are
                                        ;no attributes, return #f, not '()
(define (get-sax-atts-list sax-atts)
  (define-generic-java-methods
    get-length
    get-local-name
    get-value
    (get-uri |getURI|))
  (let ((natts (->number (get-length sax-atts))))
    (if (= natts 0)
        #f
        (let loop ((al '())
                   (n 0))
          (if (>= n natts)
              (reverse al)
              (let ((nj (->jint n)))
                (loop (cons (list (qname (get-uri sax-atts nj)
                                         (get-local-name sax-atts nj))
                                  (->string (get-value sax-atts nj)))
                            al)
                      (+ n 1))))))))

                                        ;Given a java string, return scheme #t
                                        ;if the string matches all-whitespace
(define string-all-whitespace?
  (let ((pattern #f))
    (define-java-class <java.util.regex.pattern>)
    (define-generic-java-methods compile matcher matches)
    (lambda (jstr)
      (or pattern
          (set! pattern (compile (java-null <java.util.regex.pattern>)
                                 (->jstring "^\\s*$"))))
      (->boolean (matches (matcher pattern jstr))))))

                                        ;Make a proxy SAX ContentHandler
                                        ;intereface, using the given
                                        ;ssax-accumulator.  If SKIP-WHITESPACE?
                                        ;is true, then omit any all-whitespace
                                        ;strings.
(define-java-proxy (make-content-handler-proxy ssax-accumulator)
  (<org.xml.sax.content-handler>)
  (define (characters h ch start length)
    (define-java-class <java.lang.string>)
    (let ((js (java-new <java.lang.string> ch start length)))
      (if (and (sexp-xml:skip-whitespace?)
               (string-all-whitespace? js))
          (null-method 'characters)
          (ssax-accumulator 'characters (->string js)))))
  (define (end-document h)
    (ssax-accumulator 'end-document))
  (define (end-element h namespace-uri local-name q-name)
    (ssax-accumulator 'end-element))
  (define (end-prefix-mapping h prefix)
    (null-method 'end-prefix-mapping))
  (define (ignorable-whitespace h ch start length)
    (if (sexp-xml:skip-whitespace?)
        (null-method 'ignorable-whitespace)
        (ssax-accumulator 'characters 
                          (->string
                           (java-new <java.lang.string> ch start length)))))
  (define (processing-instruction h target data)
    (ssax-accumulator 'processing-instruction (->string target) (->string data))
    (null-method 'processing-instruction))
  (define (set-document-locator h locator)
    (null-method 'set-document-locator))
  (define (skipped-entity h name)
    (null-method 'skipped-entity))
  (define (start-document h)
    (ssax-accumulator 'start-document))
  (define (start-element h namespace-uri local-name q-name atts)
    (let ((ns (->string namespace-uri)))
      (ssax-accumulator 'start-element
                        namespace-uri
                        local-name
                        atts)))
  (define (start-prefix-mapping h prefix uri)
    (null-method 'start-prefix-mapping)))

(define (sexp-xml:xml->sexp/file filename)
  (define-java-classes
    <org.xml.sax.input-source>
    <java.io.file-reader>)
  (xml->sexp/input-source (java-new <org.xml.sax.input-source>
                                    (java-new <java.io.file-reader>
                                              (->jstring filename)))))

(define (sexp-xml:xml->sexp/string string)
  (define-java-classes
    <org.xml.sax.input-source>
    <java.io.string-reader>)
  (xml->sexp/input-source (java-new <org.xml.sax.input-source>
                                    (java-new <java.io.string-reader>
                                              (->jstring string)))))

(define (sexp-xml:xml->sexp/reader java-reader)
  (define-java-class
    <org.xml.sax.input-source>)
  (xml->sexp/input-source (java-new <org.xml.sax.input-source> java-reader)))
 
;; Procedure: XML->SEXP/INPUT-SOURCE
;; Read XML from the given SAX InputSource, returning a SExp.
(define (xml->sexp/input-source input-source)
  (define-generic-java-method parse)
  (let* ((sa (new-ssax-accumulator))
         (xml-reader (get-xml-reader-with-proxy
                      (make-content-handler-proxy sa))))
    ;; Complicated call to (parse xml-reader input-source), to unpack and
    ;; rethrow any errors.  A Scheme error (such as undefined symbols
    ;; or the wrong number of arguments) seems to cause the proxy to throw
    ;; java.lang.reflect.UndeclaredThrowableException, which
    ;; needs special handling.  I think I've done this correctly by
    ;; checking that SAX-EXCEPTION, below, is a SAXException in fact,
    ;; and handling it using just Exception methods if not, but I'm
    ;; still not positive I've got this right.
    (with/fc (lambda (m e)
               (define-generic-java-methods
                 get-message
                 get-system-id
                 get-line-number
                 get-column-number
                 is-instance)
               (define-java-classes
                 (<sax-exception> |org.xml.sax.SAXException|))
               (let ((sax-exception (error-message m)))
                 (if (->boolean (is-instance <sax-exception> sax-exception))
                     (let ((sysid   (get-system-id sax-exception))
                           (line-no (->number (get-line-number sax-exception)))
                           (col-no  (->number
                                     (get-column-number
                                      sax-exception)))
                           (msg (->string (get-message sax-exception))))
                       (error 'xml->sexp/input-source
                              "parse error:~a:~a:~a: ~a"
                              (if (java-null? sysid) "?" sysid)
                              (if (< line-no 0) "?" line-no)
                              (if (< col-no 0)  "?" col-no)
                              msg))
                     (let ()
                       (define-generic-java-methods get-cause)
                       (error 'xml->sexp/input-source
                              "Scheme? error: ~a"
                              (->string (get-cause sax-exception)))))))
       (lambda () (parse xml-reader input-source)))
    (sa '*TOP*)))

(define (get-xml-reader-with-proxy content-handler-proxy)
  (define-java-classes
    (<sax-parser-factory> |javax.xml.parsers.SAXParserFactory|))
  (define-generic-java-methods
    new-instance
    (new-sax-parser |newSAXParser|)
    (get-xml-reader |getXMLReader|)
    set-content-handler
    set-feature)
  (let ((reader (get-xml-reader
                 (new-sax-parser
                  (new-instance (java-null <sax-parser-factory>))))))
    (set-content-handler reader
                         content-handler-proxy)
    ;; the following should be the defaults
    (set-feature reader
                 (->jstring "http://xml.org/sax/features/namespaces")
                 (->jboolean #t))
    (set-feature reader
                 (->jstring "http://xml.org/sax/features/namespace-prefixes")
                 (->jboolean #f))
    reader))

;; Parameter procedure: SEXP-XML:SKIP-WHITESPACE?
;; This is a SRFI-39 parameter.  It is a procedure which returns
;; a boolean value indicating whether all-whitespace content read within
;; the input XML is skipped (if true) or included verbatim (if #f).  Since
;; it is a parameter, it may be manipulated using the SRFI-39 PARAMETERIZE
;; special form.
(define sexp-xml:skip-whitespace?
  (make-parameter #t))

(define sexp-xml:localnames-into-symbols?
  (make-parameter #t))

;; SEXP-XML:SEXP->XML list [block-elements [para-elements]]
;;   => string
;; SEXP-XML:SEXP->XML port list [block-elements [para-elements]]
;;  => <undef>, but XML is written to port
;;
;; Given a list of SExps, SEXP-LIST, return this translated into a string
;; Takes two optional arguments: the first specifies a list of
;; elements which are to be formatted (ie, have linebreaks inserted)
;; as `block' elements (like <div> in HTML), and the second a list
;; which should be formatted as `para' elements (like HTML <p>).
;; Either may be given as 'ALL to format all like this.
;;
;; If the second element of the list is of the form (@ LIST ...), then the
;; LIST is a two-element list of (ATTRIBUTE VALUE)
(define (sexp-xml:sexp->xml arg . args)
  (if (output-port? arg)
      (if (null? args)
          (error "sexp->xml: no sexp argument found")
          (apply sexp->xml* (cons arg args)))
      (let ((string-port (open-output-string)))
        (apply sexp->xml* `(,string-port ,arg ,@args))
        (get-output-string string-port))))

(define (sexp->xml* port s . opts)
  (let ((block-elems (and (> (length opts) 0)
                          (car opts)))
        (para-elems  (and (> (length opts) 1)
                          (cadr opts))))
    (cond
     ((string? s)
      (display s port))

     ((symbol? s)
      (display s port))

     ((number? s)
      (format port "~a" s))

     ((list? s)
      (if (and (> (length s) 1)
               (list? (cadr s))
               (eq? (caadr s) '@))
          (sexp->xml-write* port
                            (car s)
                            (cdadr s)
                            (cddr s)
                            block-elems
                            para-elems)
          (sexp->xml-write* port
                            (car s)
                            #f
                            (cdr s)
                            block-elems
                            para-elems)))
     (else
      (error (format #f
                      "Unrecognised type of object (~s) in sexp-xml:sexp->xml"
                      s))))))


;; Write out an element with attributes, and formatting depending on the
;; element `type'.
;; GI: a symbol containing the name of the element to be written
;; ATTLIST: a list of two-element lists, each containing (attribute value),
;;     as either symbols or strings
;; CONTENT: a sexp representing the element content
;; BLOCK-ELEMENT-LIST and PARA-ELEMENT-LIST: either a list of symbols
;;     or the symbol 'ALL.  If the GI is found in one of the lists, or the
;;     relevant variable has the value 'ALL, then the element is formatted
;;     as a block element, a paragraph element, or an inline element if it
;;     is in neither list.
;; Internal function
(define (sexp->xml-write* port
                          gi attlist content
                          block-element-list para-element-list)
  (define block-elements
    (or block-element-list
        '(html head body div ul ol)))
  (define para-style
    (or para-element-list
        '(p title link h1 h2 h3 h4 h5 h6 li)))
  (cond ((eq? gi '*PI*)
         (format port "<?~a?>" (apply string-append content)))
        ((eq? gi '*CDATA*)
         (format port "<![CDATA[~a]]>" (apply string-append content)))
        ((null? content)
         (format port "<~a" gi)
         (if attlist
             (for-each (lambda (p)
                         (format port " ~a='~a'" (car p) (cadr p)))
                       attlist))
         (if (or (eq? block-elements 'ALL)
                 (memq gi block-elements)
                 (eq? para-style 'ALL)
                 (memq gi para-style))
             (format port "/>~%")
             (format port "/>")))
        (else ; has list content
         (format port "<~a" gi)
         (if attlist
             (for-each (lambda (p)
                         (format port " ~a='~a'" (car p) (cadr p)))
                       attlist))
         (if (or (eq? block-elements 'ALL)
                 (memq gi block-elements))
             (format port ">~%")
             (format port ">"))
         (for-each (lambda (x)
                     (sexp->xml* port x block-element-list para-element-list))
                   content)
         (format port
                  (cond ((or (eq? block-elements 'ALL)
                             (memq gi block-elements))
                         "</~a>~%~%")
                        ((or (eq? para-style 'ALL)
                             (memq gi para-style))
                         "</~a>~%")
                        (else
                         "</~a>"))
                  gi))))

;; SEXP-XML:ESCAPE-STRING-FOR-XML string -> string
;; SEXP-XML:ESCAPE-STRING-FOR-XML string port -> <undef>
;;
;; Given a string, return the string with < and & characters escaped.
;;
;; If the optional port argument is present, write to the given port instead.
;; PORT may be a port, or #t, in which case output is to the
;; (current-output-port), or #f, in which case the result is returned
;; as a string.
(define (sexp-xml:escape-string-for-xml s . opt-port)
  (define (write-while-escaping l port)
    (if (not (null? l))
        (let ((c (car l)))
          (cond ((char=? c #\<)
                 (display "&lt;" port))
                ((char=? c #\&)
                 (display "&amp;" port))
                ((char=? c #\>)
                 (display "&gt;" port)) ;for symmetry
                (else
                 (display c port)))
          (write-while-escaping (cdr l) port))))
  (cond ((or (null? opt-port)           ; return a string
             (not (car opt-port)))      ; argument is #f
         (let ((sp (open-output-string)))
           (write-while-escaping (string->list s) sp)
           (get-output-string sp)))
        ((output-port? (car opt-port))         ; send it to the given port
         (write-while-escaping (string->list s) (car opt-port)))
        (else ; not port, but true (should be just #t), so send to curr. output
         (write-while-escaping (string->list s) (current-output-port)))))

)

