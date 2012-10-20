;; test cases for the xmlrpc module

(import s2j)

(require-library 'sexp-xml)
(import* sexp-xml sexp-xml:xml->sexp/string)
(require-library 'xmlrpc)
(import xmlrpc)



(define (new-call s)
  (xmlrpc:new-call (sexp-xml:xml->sexp/string s)))

;; A call which is tested below
(define simple-call
  (new-call
"<methodCall>
  <methodName>examples.getStateName</methodName>
  <params>
    <!--  1 --><param><value>string</value></param>
    <!--  2 --><param><value><string>s2</string></value></param>
    <!--  3 --><param><value><i4>10</i4></value></param>
    <!--  4 --><param><value><int>20</int></value></param>
    <!--  5 --><param><value><double>1.5</double></value></param>
    <!--  6 --><param><value><boolean>0</boolean></value></param>
    <!--  7 --><param><value><dateTime.iso8601>19980717T14:08:55</dateTime.iso8601></value></param>
    <!--  8 --><param><value><base64>eW91IGNhbid0IHJlYWQgdGhpcyE=</base64></value></param>
    <!--  9 --><param><value><struct>
      <member><name>n1</name><value>v1</value></member>
      <member><name>n2</name><value><boolean>0</boolean></value></member>
      <member><name>n3</name><value><double>99.9</double></value></member>
    </struct></value></param>
    <!-- 10 --><param><value><array><data>
      <value><i4>12</i4></value>
      <value><string>Egypt</string></value>
      <value><boolean>0</boolean></value>
      <value><i4>-31</i4></value>
    </data></array></value></param>                         
  </params>
</methodCall>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parsing of requests

(expect xmlrpc-simple-test              ;check predicate
        #t
        (xmlrpc:call? simple-call))
(expect xmlrpc-simple-notest            ;should return #f without error
        #f
        (xmlrpc:call? "not a call"))
(expect xmlrpc-simple-name              ;extract method name
        'examples.getStateName
        (xmlrpc:method-name simple-call))
(expect xmlrpc-simple-length            ;extract number of parameters
        10
        (xmlrpc:number-of-params simple-call))
(let-syntax
    ((ptest (syntax-rules ()
              ((_ n exp)
               (expect ("xmlrpc-simple-arg" n)
                       exp
                       (xmlrpc:method-param simple-call n))))))
  (ptest 1 "string")
  (ptest 2 "s2")
  (ptest 3 10)
  (ptest 4 20)
  (ptest 5 1.5)
  (ptest 6 #f)
  (ptest 7 "19980717T14:08:55")
  (ptest 8 "eW91IGNhbid0IHJlYWQgdGhpcyE=")
  (ptest 9 '(("n1" . "v1") ("n2" . #f) ("n3" . 99.9)))
  (ptest 10 '#(12 "Egypt" #f -31)))

(expect xmlrpc-simple-paramlist-1
        10
        (length (xmlrpc:method-param-list simple-call)))
(expect xmlrpc-simple-paramlist-2
        "string"
        (car (xmlrpc:method-param-list simple-call)))

(expect-failure xmlrpc-simple-fail0     ;index out of range
                (xmlrpc:method-param simple-call 0))
(expect-failure xmlrpc-simple-fail11    ;index out of range
                (xmlrpc:method-param simple-call 11))

;; Procedure to call xmlrpc:new-call with the given <param> content string
(define (make-with-param-value param-content-string)
  (new-call (format #f "<methodCall><methodName>x</methodName><params><param><value>~a</value></param></params></methodCall>"
                    param-content-string)))
(expect-failure xmlrpc-fail-badboolean  ;2 is not a boolean
                (make-with-param-value "<boolean>2</boolean>"))
(expect-failure xmlrpc-fail-badname     ;struct member name must be a string
                (make-with-param-value "<struct><member><name><int>1</int></name><value>v</value></member></struct>"))

(expect-failure xmlrpc-malformed        ;malformed input
                (make-with-param-value "<wibble/>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generation of responses

(expect xmlrpc-response-string          ;response with string argument
        '(|methodResponse| (params (param (value (string "South Dakota")))))
        (xmlrpc:create-response "South Dakota"))
(expect xmlrpc-response-escapable-string ;string argument with XML chars
        '(|methodResponse| (params (param (value (string "1 &lt; 2 &amp; 3")))))
        (xmlrpc:create-response "1 < 2 & 3"))
(expect xmlrpc-response-format          ;response with formatted string
        '(|methodResponse| (params (param (value (string "number 1")))))
        (xmlrpc:create-response "number ~a" 1))
(expect xmlrpc-response-int             ;...int argument
        '(|methodResponse| (params (param (value (int "99")))))
        (xmlrpc:create-response 99))
(expect xmlrpc-response-double          ;...double argument
        '(|methodResponse| (params (param (value (double "1.5")))))
        (xmlrpc:create-response 1.5))
(expect xmlrpc-response-boolean         ;...boolean argument
        '(|methodResponse| (params (param (value (boolean "0")))))
        (xmlrpc:create-response #f))
(expect-failure xmlrpc-fail-badtype     ;impossible type for XML-RPC response
                (xmlrpc:create-response (lambda () "x")))

(expect xmlrpc-response-fault1
        '(|methodResponse|
          (fault
           (value
            (struct
             (member (name "faultCode")
                     (value (int "0")))
             (member (name "faultString")
                     (value (string "error1&amp;more")))))))
        (xmlrpc:create-fault 0 "error1&more")) ;simple format string -- no args
(expect xmlrpc-response-fault2
        '(|methodResponse|
          (fault
           (value
            (struct
             (member (name "faultCode")
                     (value (int "1")))
             (member (name "faultString")
                     (value (string "fmt: 1/#t")))))))
        (xmlrpc:create-fault 1 "fmt: ~a/~a" 1 #t)) ;format string with args

