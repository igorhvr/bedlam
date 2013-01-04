; sisc-ssax.scm

(require-library 'sisc/libs/srfi/srfi-13)
(import srfi-13)
(import string-io)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define-syntax inc!
   (syntax-rules ()
        ((inc! x) (set! x (+ x 1)))))

(define-syntax dec!
   (syntax-rules ()
        ((dec! x) (set! x (- x 1)))))

(define-syntax env.bind
   (syntax-rules () ((env.bind key value) (%%env.bind 'key value))))

(define-syntax env.find
   (syntax-rules () ((env.find key) (%%env.find 'key))))

(define-syntax env.demand
   (syntax-rules () ((env.demand key) (%%env.demand 'key))))

(define (ssax:warn port msg . other-msg)
   (apply cerr (cons* nl "Warning: " msg other-msg)))

(define namespaces '())

;;;;;;;;;; LOAD ;;;;;;;;;;

(current-directory "c:/sisc/contrib/pure-scheme/ssax/")
;;
(load "myenv-sisc.scm")

(current-directory "c:/SSAX/lib/")
;;
(load "char-encoding.scm")
(load "define-opt.scm")
(load "catch-error.scm")
(load "srfi-13-local.scm")
(load "util.scm")
(load "look-for-str.scm")
(load "input-parse.scm")
(load "env.scm")
;(load "SSAX.scm")
(load "SSAX-code.scm")
(load "SXML-tree-trans.scm")
(load "SXML-to-HTML.scm")
(load "SXML-to-HTML-ext.scm")

 ;;;;;;;;;; TEST ;;;;;;;;;;

(current-directory "../tests/")
;;
(load "vmyenv.scm")
(if (failed? (load "vinput-parse.scm"))
       (begin
              (display "************* vinput-parse.scm failed ******************\n")))
(load "vSXML-tree-trans.scm")
(load "vSXML-to-HTML.scm")

;;;;;;;;;; EXAMPLES ;;;;;;;;;;

(current-directory "../examples/")
;;
(load "remove-markup.scm")
(display
  (call-with-input-file "xml/ddn.rdf" remove-markup))
(load "outline.scm")
(call-with-input-file "xml/total_weather.xsl" outline)
(call-with-input-file "xml/OMF-sample.xml"
   (lambda (port) (ssax:xml->sxml port '())))
(load "html-parser.scm")
;;(load "apply-templates.scm")
(load "pull-punct-sxml.scm")
;;(load "sxml-db-conv.scm")
;;(load "daml-parse-unparse.scm")
(load "sxslt-advanced.scm")

;;;;;;;;;; SXPATH ;;;;;;;;;;

(current-directory "c:/sxml-tools/")
;;
(load "sxml-tools.scm")
(load "sxpathlib.scm")
(load "sxpath-ext.scm")
(load "xpath-parser.scm")
(load "txpath.scm")

(load "tests/xtest-harness.scm")
(load "tests/xtest-lib.scm")

;;(load "sxpath/test/sxpathlib-tst.scm")
;;(load "sxpath/test/sxpath-ext-tf.scm")
;;(load "sxpath/test/txpath-tf.scm")