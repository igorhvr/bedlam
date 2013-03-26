;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

;(begin (define iasylum-bedlam-location "/home/igorhvr/idm/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))
(require-extension (srfi 23)) ; error

;;(with-failure-continuation 
;; (lambda (e p)
;;   (display "\n\nSince we cannot find bedlam in the classpath, the variable iasylum-bedlam-location must be defined and initialized to the location of the library.\n")
;;   (display "\nExample of code to use this library.: \n")
;;   (display "   (begin (define iasylum-bedlam-location \"/home/igorhvr/idm/bedlam/\") (load (string-append iasylum-bedlam-location \"iasylum/init.scm\")))\n\n\n")
;;   (throw e))
;; (lambda ()
;;   (define tmp iasylum-bedlam-location)
;;   (string-append "test" tmp)
;;   (void)
;;   ))

;; This is to allow classpath-based loading (with the help of a custom URL Handler) -  E.g.: (load "classpath:iasylum/jdbc.scm")
(define original-load load)
(set! load
      (lambda (name)
        (with-failure-continuation 
         (lambda (e p)
           (with-failure-continuation 
            (lambda (e p)
              (with-failure-continuation
               (lambda (e p)
                 (let ((name (irregex-replace/all (irregex "file:") name "")))
                   (original-load (string-append "classpath:" name))))
               (lambda ()
                 (original-load (string-append "classpath:iasylum/" name)))))
            (lambda ()
              (original-load (string-append "classpath:" name)))))
         (lambda ()
           (with-failure-continuation 
            (lambda (e p)
              (original-load (string-append (iasylum-bedlam-location) "/" name)))
            (lambda ()
              (original-load name)))))))

(require-extension (srfi 39)) ; make-parameter

(define iasylum-bedlam-location
  (let ((param (make-parameter #f)))
    (with-failure-continuation 
     (lambda (e p) (param ""))
     (lambda () (param iasylum-bedlam-location)))
    param
    ))

;;(compile-file "/home/igorhvr/idm/dupemgr/app/WEB-INF/scm/iasylum/jcode.scm" "/home/igorhvr/idm/dupemgr/app/WEB-INF/scm/iasylum/jcode.scc")

;; IASylum

(class-path-extension-append! (cons (string-append (iasylum-bedlam-location) "java-base/") (class-path-extension)))

;; Configuration files.
(class-path-extension-append! (cons (string-append (iasylum-bedlam-location) "config/") (class-path-extension)))

  ;;; Low level - java libraries.
(define add-lib
  (lambda (name)
    (class-path-extension-append! (cons (string-append (iasylum-bedlam-location) "jars/" name) (class-path-extension)))))

(add-lib "u/bsf.jar")
(add-lib "u/js.jar")
(add-lib "u/jdbc-postgresql.jar")
(add-lib "jdbc/log4jdbc4-1.2.jar")
(add-lib "jdbc/slf4j-api-1.6.0.jar")
(add-lib "jdbc/slf4j-log4j12-1.7.2.jar")
(add-lib "jdbc/mysql-connector-java-5.1.24-bin_and_src.jar")
(add-lib "excel/poi-3.9-20121203.jar")
(add-lib "excel/poi-ooxml-3.9-20121203.jar")
(add-lib "excel/poi-examples-3.9-20121203.jar")
(add-lib "excel/poi-ooxml-schemas-3.9-20121203.jar")
(add-lib "excel/poi-excelant-3.9-20121203.jar")
(add-lib "excel/poi-scratchpad-3.9-20121203.jar")
(add-lib "sisc/sisc-heap.jar")
(add-lib "sisc/sisc-lib.jar")
(add-lib "sisc/sisc-opt.jar")
(add-lib "sisc/sisc.jar")
(add-lib "siscweb/siscweb-sql.jar")
(add-lib "siscweb/siscweb-sxml.jar")
(add-lib "siscweb/siscweb.jar")
(add-lib "u/activation-1.1.jar")
(add-lib "u/commons-email-1.2.jar")
(add-lib "u/commons-fileupload-1.2.1.jar")
(add-lib "u/commons-io-2.0.1.jar")
(add-lib "u/commons-logging-1.1.jar")
(add-lib "u/junit-3.8.1.jar")
(add-lib "u/log4j-1.2.13.jar")
(add-lib "u/m.jar")
(add-lib "u/mail.jar")
(add-lib "u/ssax-sxml.jar")

(add-lib "tomcat/jasper-el.jar")
(add-lib "tomcat/tomcat-i18n-es.jar")
(add-lib "tomcat/tomcat-i18n-fr.jar")
(add-lib "tomcat/servlet-api.jar")
(add-lib "tomcat/bootstrap.jar")
(add-lib "tomcat/commons-daemon.jar")
(add-lib "tomcat/catalina-tribes.jar")
(add-lib "tomcat/catalina-ant.jar")
(add-lib "tomcat/tomcat-coyote.jar")
(add-lib "tomcat/jasper-jdt.jar")
(add-lib "tomcat/jstl.jar")
(add-lib "tomcat/tomcat-dbcp.jar")
(add-lib "tomcat/catalina-ha.jar")
(add-lib "tomcat/jasper.jar")
(add-lib "tomcat/tomcat-i18n-ja.jar")
(add-lib "tomcat/tomcat-juli.jar")
(add-lib "tomcat/jsp-api.jar")
(add-lib "tomcat/el-api.jar")
(add-lib "tomcat/catalina.jar")
(add-lib "tomcat/annotations-api.jar")
(add-lib "tomcat/standard.jar")

(add-lib "u/quartz-all-1.7.3.jar")

(import os)
(import java-io)
(import serial-io)
(import networking) (import binary-io) (import custom-io)
(import threading) 
  ;;; IAsylum scheme files.
(class-path-extension-append! (cons (iasylum-bedlam-location) (class-path-extension)))

(max-stack-trace-depth 16)
(import debugging)
(require-extension (lib iasylum/jcode))

;; If this is called (j "new URL(\"classpath:iasylum/jdbc.scm\");") and similar things will work.
(define (setup-classpath-url-handler)
  (j "iu.BedlamBundleInit.setupClasspathURLHandler();"))

(require-extension (lib iasylum/match))
(require-extension (lib iasylum/random))
(require-extension (lib iasylum/srfi-88))
(require-extension (srfi 26)) ; cut / cute
(require-extension (lib iasylum/srfi-89))

(require-extension (srfi 1)) ; filter
                                        ; (define* (g a (b a) (key: k (* a b))) (list a b k)) ; to test

(require-extension (lib siscweb/html))
(require-extension (lib siscweb/text))
(import siscweb/html)
(import siscweb/text)
(import record)
(import hashtable)
(require-extension (lib iasylum/password))
(import siscweb/request)
(import siscweb/session)
(import string-io)
(import siscweb/bindings)
(require-extension (srfi 2))
(import s2j)
(require-extension (srfi 16)) ; case-lambda
(require-extension (srfi 13))
(require-extension (srfi 42)) ; Eager comprehensions.
(require-extension (lib iasylum/email))
(import siscweb/context)
(import siscweb/html)
(import siscweb/text)
(require-extension (lib iasylum/log))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/excel))
(require-extension (lib iasylum/i18n))
(import networking)
(import binary-io)
(import custom-io)
(import file-manipulation)
(require-extension (lib iasylum/iasylum))

;; irregex
(load "iasylum/irregex.scc")

;; let-optionals
(load "iasylum/let-optionals/let-optionals.scm")

;; SLIB.
;;(load "iasylum/slib.scm")
(load "iasylum/slib/iasylum-sisc.init")
;;(require 'new-catalog)

;; FIXXXME I am not sure with require 'line-i/o does not work, even after line-i/o
;; was added to supported features. This will do for now, providing the very useful
;; read-line.
(load "iasylum/slib/3b2/lineio.scm")

;(display "\n\nLOADED iasylum-bedlam.\n\n")

(begin
;; Jackcess and required libraries.
(add-lib "microsoft_access/commons-lang-2.5.jar")
(add-lib "microsoft_access/jackcess-1.2.9.jar")
)

(add-lib "u/junrar-0.7-SNAPSHOT.jar")

(add-lib "clojure/clojure-1.5.0-master-SNAPSHOT.jar")
(add-lib "clojure/tools.nrepl-0.2.0-SNAPSHOT.jar")

(define (extend-classpath i)
  (class-path-extension-append! (cons i (class-path-extension))))

(define (find-jars-in-directory dir)
  (map car (filter (lambda (v) (not (eqv? v '()))) (map (lambda (v) (irregex-extract (irregex '(seq (* any) ".jar")) v)) (rglob dir)))))

(define (add-jars-in-directory-to-classpath dir)
  (for-each
   extend-classpath
   (find-jars-in-directory dir))
  (class-path-extension))

(define (gen-classpath-str dir)
  (apply string-append
         (map
          (lambda (v)
            (string-append "export CLASSPATH=$CLASSPATH:" v " ; "))
          (find-jars-in-directory dir))))

(require-extension (srfi 1 6 13 23 66 69))
;;(require-extension (lib iasylum/fmt)) ; Disabled because of
;;{warning: naked left-hand reference in letrec rhs: '%%_FXYoD_hd61_pad'.}
;;{warning: naked left-hand reference in letrec rhs: '%%_FXa1-0dd61_make-string-fmt-transformer'.}
;; and similar issues, still to be debugged.

 (define (force-fmt-load)
    (load "iasylum/fmt/fmt-0.8.1/let-optionals.scm")  ; if you don't have LET-OPTIONALS*
    (load "iasylum/fmt/fmt-0.8.1/read-line.scm")      ; if you don't have READ-LINE
    (load "iasylum/fmt/fmt-0.8.1/string-ports.scm")   ; if you don't have CALL-WITH-OUTPUT-STRING
    (load "iasylum/fmt/fmt-0.8.1/make-eq-table.scm")
    (load "iasylum/fmt/fmt-0.8.1/mantissa.scm")
    (load "iasylum/fmt/fmt-0.8.1/fmt.scm")
    (load "iasylum/fmt/fmt-0.8.1/fmt-pretty.scm")     ; optional pretty printing
    (load "iasylum/fmt/fmt-0.8.1/fmt-column.scm")     ; optional columnar output
    (load "iasylum/fmt/fmt-0.8.1/fmt-c.scm")          ; optional C formatting utilities
    (load "iasylum/fmt/fmt-0.8.1/fmt-color.scm")      ; optional color utilities
    (load "iasylum/fmt/fmt-0.8.1/fmt-js.scm")         ; javascript utilities. 
    (load "iasylum/fmt/fmt-0.8.1/fmt-unicode.scm"))         ; javascript utilities.

;; FIXXXME Hack that will be used until I debug the naked left-hand reference issues above.
(force-fmt-load)


(add-lib "jdbc/jtds-1.2.5.jar")

;; com.eaio.uuid.UUID u = new com.eaio.uuid.UUID();
;; From http://johannburkard.de/software/uuid/
(add-lib "u/uuid-3.3.jar")

;; Force class loading.
(j "iu.M.d;")

(require-extension (lib iasylum/clojure))

(define nrepls
  (lambda* ((beanshell-httpd-port: beanshell-httpd-port 3001)
       (beanshell-vanilla-port: beanshell-vanilla-port 3002)
       (sisc-repl-port: sisc-repl-port 3000)
       (clojure-nrepl-default-transport-port: clojure-nrepl-default-transport-port 6000)
       (clojure-nrepl-tty-transport-port: clojure-nrepl-tty-transport-port 6001))

      (if (and (eqv?  beanshell-httpd-port 3001) (eqv? beanshell-vanilla-port 3002) (eqv? sisc-repl-port 3000)
               (eqv? clojure-nrepl-default-transport-port 6000) (eqv?  clojure-nrepl-tty-transport-port 6001))
          (begin (clojure/repl-start 6000) (j "iu.M.i();") "Starting repls at 3000 (SISC), 3001 (beanshell httpd), 3002 (beanshell), 6000 (clojure nrepl), 6001 (clojure tty transport)..." )
          (error "nrepls: using any ports other than the default is not yet supported."))))

(define (start-bsh-service portnum)
  (j "		bsh.Interpreter i = new bsh.Interpreter();
		i.set( \"data\", iu.M.d );
		i.set( \"portnum\", portnum );  
		i.eval(\"setAccessibility(true)\");
		i.eval(\"show()\");
		i.eval(\"server(portnum)\");" `((portnum ,(->jint portnum)))))

	     
;; FIXME - this is not working currently.
(define (start-sisc-service portnum)
  (let* ((addr (j "java.net.InetAddress.getByName(\"localhost\");"))
          (server-socket (j "new ServerSocket(port, 50, addr);" `((addr ,addr) (port ,(->jint portnum))))))
    (j "new Thread() {
                  public void run() {
                      try {
                          appcontext = new sisc.interpreter.AppContext()
                          appcontext.addDefaultHeap();
                          dynenv = new sisc.env.DynamicEnvironment(appcontext, System.in, System.out);
                          interpreter = new sisc.interpreter.Interpreter(new sisc.interpreter.ThreadContext(), dynenv);
                          dynenv.bind();
                          sisc.REPL.listen(interpreter.getCtx(), serversocket);
                      } catch (Exception e) {
                          throw new RuntimeException(e);
                      }
                  }
              }.start();" `((serversocket ,server-socket)))))
          
;; and-let*. e.g.:
;; (and-let* ((v (assoc  "subscription_id" '(("subscription_id" ":db/unique    :db.unique/identity")) ))) (cdr v))
(require-extension (srfi 2))

;; Irresistibly horrible.
;; Source: http://okmij.org/ftp/Scheme/setf.txt
(define-macro (setf! F V)
  ;; symbol->string chopping off a trailing -ref if any
  (define (-ref-less sym)
    (let* ((str (symbol->string sym)) (suffix "-ref")
           (s-pos (- (string-length str) (string-length suffix))))
      (if (negative? s-pos) str
        (let loop ((i 0))
             (cond
              ((>= i (string-length suffix)) (substring str 0 s-pos))
              ((char=? (string-ref suffix i) (string-ref str (+ i s-pos)))
               (loop (+ 1 i)))
              (else str))))))

  (if (not (pair? F)) `(set! ,F ,V)
    (case (car F)
          ((car) `(set-car! ,@(cdr F) ,V))
          ((cdr) `(set-cdr! ,@(cdr F) ,V))
          ((cadr) `(setf! (car (cdr ,@(cdr F))) ,V))
          ((cddr) `(setf! (cdr (cdr ,@(cdr F))) ,V))
                ; I need to handle other cadda..vers but I'm tired...
          (else `(,(string->symbol (string-append (-ref-less (car F)) "-set!"))
                  ,@(cdr F) ,V)))))


;; sxml bootstrap
(require-library 'ssax/ssax)
(require-library 'sxml/sxml-to-xml)
(require-library 'ssax/ssax)
(require-library 'sxml/sxml-to-xml)
(import ssax)
(import sxml/sxml-to-xml)
(require-library 'sisc/ssax-sxml/ssax-sxml-code)
(require-library 'sisc/ssax-sxml/ssax-sxml-code)
(require-library 'sisc/ssax-sxml/sxml-match)
(require-library 'sisc/ssax-sxml/query)
(import srfi-1-filter)
(import string-io)
(import myenv)
(import util)
(import srfi-12)
(import srfi-13)
(import parse-error)
(import input-parse)
(import look-for-str)
(import char-encoding)
(import ssax-code)
(import sxml-tree-trans)
(import id)
(import http)
(import htmlprag)
(import access-remote)
(import sxpathlib)
(import sxml-tools)
(import sxpath-ext)
(import xpath-parser)
(import xpath-ast)
(import xlink+xpath-context)
(import lazy-xpath)
(import lazy-ssax)
(import mdl:sxpath)
(import mdl:txpath)
(import libmisc)
(import fragments)
(import modif)
(import ddo-axes)
(import ddo-txpath)
(import stx-engine)
(import sxml-matcher)
(import serializer)
(import query)

;; This would run all tests: (require-library 'sisc/ssax-sxml/doc/tests/all-tests)


(require-extension (lib iasylum/match))
(require-extension (lib iasylum/jdbc))
(require-extension (lib iasylum/json))

(set! load original-load)
