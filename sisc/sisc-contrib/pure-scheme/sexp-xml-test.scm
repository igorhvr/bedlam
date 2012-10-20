;; Test cases for the sexp-xml.scm package

(require-library 'sisc/libs/srfi/srfi-39) ;parameter objects: form PARAMETERIZE

(require-library 'sexp-xml)
(import sexp-xml)



(expect-failure xml->sexp-1
        (sexp-xml:xml->sexp/string ""))

(expect xml->sexp-basic
        '(*TOP* (doc (p "hello") (p)))
        (sexp-xml:xml->sexp/string "<doc><p>hello</p><p/></doc>"))

(expect xml->sexp-basic-with-spaces
        '(*TOP* (doc (p "hello  ") (p)))
        (sexp-xml:xml->sexp/string "<doc>  <p>hello  </p>  <p>   </p> </doc>"))

(expect xml->sexp-basic-plus-whitespace
        '(*TOP* (doc "  " (p "hello  ") "  " (p "   ") " "))
        (parameterize ((sexp-xml:skip-whitespace? #f))
          (sexp-xml:xml->sexp/string
           "<doc>  <p>hello  </p>  <p>   </p> </doc>")))

;; The order of attributes is preserved by xml->sexp, though it's not
;; thus documented.
(expect xml->sexp-attributes
        '(*TOP* (doc (p (@ (class "simple"))
                        "content1")
                     (p (@ (class "complicated")
                           (att1  "value1")))
                     (p "content2")))
        (sexp-xml:xml->sexp/string "<doc><p class='simple'  >content1</p>  <p class='complicated' att1=\"value1\"/> <p>content2</p></doc>"))

(expect xml->sexp-namespacing
        '(*TOP* (doc (p (@ (("urn:example" . a1) "v1")
                           (a2 "v2"))
                        "c1")
                     (("urn:example" . q)
                      "c2")
                     (("urn:example2" . r)
                      "c3")))
        (sexp-xml:xml->sexp/string
         "<doc xmlns:x='urn:example'><p x:a1='v1' a2='v2'>c1</p><x:q>c2</x:q><r xmlns='urn:example2'>c3</r></doc>"))

(expect xml->sexp-reader
        '(*TOP* (doc (p (@ (class "simple")
                           (("urn:example" . a1) "v1")))))
        (define-java-class <java.io.string-reader>)
        (sexp-xml:xml->sexp/reader
         (java-new <java.io.string-reader>
                   (->jstring
                    "<doc>  <p class='simple' x:a1='v1' xmlns:x='urn:example'/></doc>"))))

(expect xml->sexp-qname-cases
        '(*TOP* (doc (p (@ (class "SimPle")
                           (("urn:Example1" . a1) "v1")))
                     (p "CONTENT1")))
        (sexp-xml:xml->sexp/string
         "<Doc><P Class='SimPle' xmlns:x='urn:Example1' x:a1='v1'/><p>CONTENT1</p></Doc>"))

(expect xml->sexp-qname-cases2
        '(*TOP* ("Doc" ("P" (@ ("Class" "SimPle")
                               (("urn:Example1" . "a1") "v1")))
                 ("p" "CONTENT1")))
        (parameterize ((sexp-xml:localnames-into-symbols? #f))
          (sexp-xml:xml->sexp/string
           "<Doc><P Class='SimPle' xmlns:x='urn:Example1' x:a1='v1'/><p>CONTENT1</p></Doc>")))

(expect sexp->xml-simple
        "<p>hello <em>there</em></p>\n"
        (sexp-xml:sexp->xml '(p "hello " (em there))))

(expect sexp->xml-pi
        "<p>hello<?xml version='1.0'?></p>\n"
        (sexp-xml:sexp->xml '(p "hello" (*PI* "xml version='1.0'"))))
(expect sexp->xml-cdata
        "<p>hello <![CDATA[ping<&and stuff]]></p>\n"
        (sexp-xml:sexp->xml '(p "hello " (*CDATA* "ping<&" "and stuff"))))

(expect escape-string-for-xml
        "hello&lt;there&amp;again&gt;"
        (sexp-xml:escape-string-for-xml "hello<there&again>"))

