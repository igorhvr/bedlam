;; Functions to handle XML-RPC
;; See spec at <http://www.xmlrpc.com/spec>
;;
;; Procedures defined:
;;
;;   xmlrpc:new-call SEXP
;;       Returns a call object by parsing the XML-RPC method call
;;       represented by the given SSAX-style S-expression, such as
;;       either '(methodCall (methodName "x") (params (param (value "y")))),
;;       or '(*TOP* (methodCall ...)).
;;
;;   xmlrpc:call? CALL
;;       Returns #t if the object is one of the objects returned by
;;       XMLRPC:NEW-CALL, and #f otherwise.
;;
;;   xmlrpc:method-name CALL
;;       Returns the method name in the given CALL, as a Scheme symbol.
;;
;;   xmlrpc:method-param CALL INDEX
;;       Returns the INDEX'th parameter in the given CALL (1-based).
;;       Throws an error if the INDEX is not in the range
;;       [1..nparams].  The parameter values are returned as the
;;       corresponding Scheme types, with <struct> elements being
;;       returned as an alist (("member-name" <member-value>) ...),
;;       and <array> elements as a vector #(<value> ...).
;;
;;   xmlrpc:method-param-list CALL
;;       Returns the complete set of parameters as a list.
;;
;;   xmlrpc:number-of-params CALL
;;       Return the number of parameters in the given CALL.
;;
;;   xmlrpc:create-response VALUE
;;       Create an XML-RPC response wrapping the VALUE.  The response
;;       is of a type appropriate to the VALUE.  If the VALUE is a
;;       string, then it's interpreted as a format string, and the
;;       trailing arguments are formatted into it.  Returns a
;;       sexp ready to be converted to XML.
;;
;;   xmlrpc:create-fault FAULT-CODE ERROR-MESSAGE-FORMAT ARGUMENTS ...
;;       Create an XML-RPC fault response by applying the given format to
;;       the arguments.  Returns a sexp ready to be converted to XML,
;;       for example '(methodResponse (params (param (value (string "x")))))


(require-library 'sisc/libs/srfi/srfi-6)  ;basic string ports
(require-library 'sisc/libs/srfi/srfi-13) ;string libraries

(module xmlrpc
(xmlrpc:new-call
 xmlrpc:method-name
 xmlrpc:method-param
 xmlrpc:method-param-list
 xmlrpc:number-of-params
 xmlrpc:call?
 xmlrpc:create-response
 xmlrpc:create-fault)

(import* srfi-13 string-downcase)
(import srfi-6)

(define (xmlrpc:new-call call-sexp)
  (if (and call-sexp
           (list? call-sexp)
           (not (null? call-sexp)))
      (cons '*XMLRPC* (evaluate-call-sexp
                       (if (eq? (car call-sexp) '*TOP*)
                           (cadr call-sexp)
                           call-sexp)))
      (error 'xmlrpc:new-call "Malformed request: ~s" call-sexp)))

(define (xmlrpc:call? object)
  (and (pair? object)
       (eq? (car object) '*XMLRPC*)
       (pair? (cdr object))))

;; Return the name of the given CALL.
(define (xmlrpc:method-name call)
  (check-is-call-or-error 'xmlrpc:method-name call)
  (cadr call))

;; Return parameter number IDX from the given CALL.  The indexing is one-based.
(define (xmlrpc:method-param call idx)
  (check-is-call-or-error 'xmlrpc:method-param call)
  (let ((param-vec (cddr call)))
    (if (or (< idx 1) (> idx (vector-length param-vec)))
        (error 'xmlrpc:method-param
               "Bad index ~a: should be 1..~a"
               idx (vector-length param-vec)))
    (vector-ref param-vec (- idx 1))))

(define (xmlrpc:method-param-list call)
  (check-is-call-or-error 'xmlrpc:method-param-list call)
  (vector->list (cddr call)))

(define (xmlrpc:number-of-params call)
  (check-is-call-or-error 'xmlrpc:number-of-params call)
  (vector-length (cddr call)))

;; Create a response sexp of the appropriate type for the VALUE.  If
;; the VALUE is a string, then it's interpreted as a format string,
;; and the trailing arguments are formatted into it.
(define (xmlrpc:create-response value . args)
  ;; Eg:
  ;; <methodResponse>
  ;;   <params>
  ;;     <param><value><string>South Dakota</string></value></param>
  ;;  </params>
  ;; </methodResponse>
  (let ((v (cond ((string? value)       ;currently handles dates and base64, too
                  `(string ,(escape-string-for-xml
                             (apply format (cons #f (cons value args))))))
                 ((integer? value)
                  `(int ,(number->string value)))
                 ((number? value)
                  `(double ,(number->string value)))
                 ((boolean? value)
                  `(boolean ,(if value "1" "0")))
                 (else
                  (error 'xmlrpc:create-response
                         "Unexpected type: ~s" value)))))
    `(|methodResponse| (params (param (value ,v))))))

(define (xmlrpc:create-fault fault-code message-format . args)
  ;; Eg:
  ;; <methodResponse>
  ;;   <fault><value><struct>
  ;;     <member>
  ;;       <name>faultCode</name>
  ;;       <value><int>4</int></value>
  ;;     </member>
  ;;     <member>
  ;;       <name>faultString</name>
  ;;       <value><string>Too many parameters.</string></value>
  ;;     </member>
  ;;   </struct></value></fault>
  ;; </methodResponse>
  (let ((msg (escape-string-for-xml
              (apply format `(#f ,message-format ,@args)))))
    `(|methodResponse|
      (fault
       (value
        (struct
         (member
          (name "faultCode")
          (value (int ,(number->string fault-code))))
         (member
          (name "faultString")
          (value (string ,msg)))))))))

(define (check-is-call-or-error procname obj)
  (or (xmlrpc:call? obj)
      (error procname "Object ~s is not an xmlrpc call" obj)))

(define (evaluate-call-sexp sexp)
  ((eval `(lambda (methodCall
                   methodName
                   params
                   param
                   value
                   i4
                   int
                   boolean
                   string
                   double
                   dateTime.iso8601
                   base64
                   struct
                   member
                   name
                   array
                   data                   
                   )
            ,sexp)
         (scheme-report-environment 5)
         )
   do-method-call
   do-method-name
   do-params
   do-param
   do-value
   do-i4
   do-int
   do-boolean
   do-string
   do-double
   do-dateTime.iso8601
   do-base64
   do-struct
   do-member
   do-name
   do-array
   do-data
   ))
  

;; <?xml version="1.0"?>
;; <methodCall>
;;    <methodName>examples.getStateName</methodName>
;;    <params>
;;       <param>
;;          <value><i4>41</i4></value>
;;          </param>
;;       </params>
;;    </methodCall>
(define (do-method-call name params)
  (cons (string->symbol (string-downcase name))
        params))
(define (do-method-name content)
  content)
(define (do-params . content)
  (list->vector content))
(define (do-param value)
  value)
(define (do-value content)
  content)
                                        ;note that i4/int/double are all parsed
                                        ;to type <number>, without any checking
(define (do-i4 content)
  (string->number content))
(define (do-int content)
  (string->number content))
(define (do-double content)
  (string->number content))
(define (do-boolean content)            ;0->#f, 1->#t
  (let ((n (string->number content)))
    (cond ((not n)
           (error "<boolean>~a</boolean> is not a valid boolean" content))
          ((= n 0)
           #f)
          ((= n 1)
           #t)
          (else
           (error "<boolean>~a</boolean> is not a valid boolean" content)))))
(define (do-string content)
  content)
(define (do-dateTime.iso8601 content)   ;For now, just return the string.
  content)                              ;SISC has no date type: see srfi-19
(define (do-base64 content)             ;For now, just return the string.
  content)                              ;Decoders?

;; <struct>
;;   <member>
;;     <name>lowerBound</name>
;;     <value><i4>18</i4></value>
;;     </member>
;;   <member>
;;     <name>upperBound</name>
;;     <value><i4>139</i4></value>
;;   </member>
;; </struct>
(define (do-struct . content)
  content)
(define (do-member name value)
  (cons name value))
(define (do-name name)
  (if (string? name)
      name
      (error "<name>~a</name> does not have a string-valued name" name)))

;; <array>
;;   <data>
;;     <value><i4>12</i4></value>
;;     <value><string>Egypt</string></value>
;;     <value><boolean>0</boolean></value>
;;     <value><i4>-31</i4></value>
;;     </data>
;; </array>
(define (do-array content)
  (list->vector content))
(define (do-data . content)
  content)

;; ESCAPE-STRING-FOR-XML string -> string
(define (escape-string-for-xml s)
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
  (let ((sp (open-output-string)))
    (write-while-escaping (string->list s) sp)
    (get-output-string sp)))

)
