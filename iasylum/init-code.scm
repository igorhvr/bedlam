;(begin (define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))
(require-extension (srfi 23)) ; error

;;(with-failure-continuation 
;; (lambda (e p)
;;   (display "\n\nSince we cannot find bedlam in the classpath, the variable iasylum-bedlam-location must be defined and initialized to the location of the library.\n")
;;   (display "\nExample of code to use this library.: \n")
;;   (display "   (begin (define iasylum-bedlam-location \"/base/bedlam/\") (load (string-append iasylum-bedlam-location \"iasylum/init.scm\")))\n\n\n")
;;   (throw e))
;; (lambda ()
;;   (define tmp iasylum-bedlam-location)
;;   (string-append "test" tmp)
;;   (void)
;;   ))

;; This is to allow classpath-based loading (with the help of a custom URL Handler) -  E.g.: (load "classpath:iasylum/jdbc.scm")
(define original-load load)

(define bedlam-load
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

(set! load bedlam-load)

(require-extension (srfi 39)) ; make-parameter

(define iasylum-bedlam-location-parameter
  (let ((param (make-parameter #f)))
    (with-failure-continuation 
     (lambda (e p) (param ""))
     (lambda () (param iasylum-bedlam-location)))
    param
    ))

;;(compile-file "/base/dupemgr/app/WEB-INF/scm/iasylum/jcode.scm" "/base/dupemgr/app/WEB-INF/scm/iasylum/jcode.scc")

;; IASylum

(class-path-extension-append! (cons (string-append (iasylum-bedlam-location-parameter) "java-base/") (class-path-extension)))

;; Configuration files.
(class-path-extension-append! (cons (string-append (iasylum-bedlam-location-parameter) "config/") (class-path-extension)))

(define add-lib-fullpath
  (lambda (full-path)
    (class-path-extension-append! (cons full-path (class-path-extension)))))

;;; Low level - java libraries.
(define add-lib
  (lambda (name)
    (add-lib-fullpath (string-append (iasylum-bedlam-location-parameter) "jars/" name))))

(add-lib "u/bsf.jar")
(add-lib "u/guava-15.0.jar")
(add-lib "u/google-api-client-1.22.0.jar")
(add-lib "u/google-http-client-1.22.0.jar")
(add-lib "u/js.jar")
(add-lib "u/js-engine.jar")
(add-lib "u/jdbc-postgresql.jar")
(add-lib "log/slf4j-api-1.7.5.jar")
(add-lib "log/logback-core-1.0.13.jar")
(add-lib "log/log4j-core-2.0-rc2.jar")
(add-lib "log/log4j-api-2.0-rc2.jar")
(add-lib "log/log4j-1.2-api-2.0-rc2.jar")
(add-lib "log/jul-to-slf4j-1.7.5.jar")
(add-lib "log/log4j-slf4j-impl-2.0-rc2.jar")
(add-lib "javassist/javassist-rel_3_19_0_ga/javassist.jar")
(add-lib "jdbc/pool/HikariCP-2.3.2.jar")
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
(add-lib "u/commons-email-1.4.jar")
(add-lib "u/commons-exec-1.3.jar")
(add-lib "u/commons-fileupload-1.2.1.jar")
(add-lib "u/commons-io-2.0.1.jar")
(add-lib "u/commons-logging-1.1.3.jar")
(add-lib "u/junit-3.8.1.jar")
(add-lib "bsh/bsh-2.2.0.jar")
(add-lib "u/m.jar")
(add-lib "u/mail.jar")
(add-lib "u/ssax-sxml.jar")
(add-lib "u/servlet-api-3.1.jar")
(add-lib "u/prettytime-4.0.1.Final.jar")
(add-lib "u/zip4j-1.3.2.jar")

(add-lib "u/quartz-all-1.7.3.jar")
(add-lib "u/cron4j-2.2.5.jar")
(add-lib "u/json-20171018.jar")
(add-lib "u/commons-text-1.2.jar")

(add-lib "hornetq/2.4.0_Final/hornetq-core-client.jar")
(add-lib "hornetq/2.4.0_Final/hornetq-jms-client.jar")
(add-lib "hornetq/2.4.0_Final/jboss-jms-api.jar")
(add-lib "hornetq/2.4.0_Final/netty.jar")
(add-lib "hornetq/2.4.0_Final/hornetq-commons.jar")
(add-lib "hornetq/2.4.0_Final/jnp-client.jar")

;; Only one version of hornetq libraries should be in the classpath.
(add-lib "hornetq/hornetq-commons-2.3.17.Final.jar")
(add-lib "hornetq/hornetq-core-client-2.3.17.Final.jar")
(add-lib "hornetq/hornetq-journal-2.3.17.Final.jar")
(add-lib "hornetq/hornetq-server-2.3.17.Final.jar")


(import os)
(import java-io)
(import serial-io)
(import networking) (import binary-io) (import custom-io)
(import threading) 
  ;;; IAsylum scheme files.
(class-path-extension-append! (cons (iasylum-bedlam-location-parameter) (class-path-extension)))

(max-stack-trace-depth 16)
(import debugging)

;; assert & other utilities.
(require-library 'siscweb/siscweb-src-0.5/opt/sxml/scm/ssax/myenv-sisc)

(require-extension (lib iasylum/assert))

(require-extension (lib iasylum/jcode))
;(suppressed-stack-trace-source-kinds '())

;; If this is called (j "new URL(\"classpath:iasylum/jdbc.scm\");") and similar things will work.
(define (setup-classpath-url-handler)
  (j "iu.BedlamBundleInit.setupClasspathURLHandler();"))

(with-failure-continuation 
 (lambda (e p)
   (list 'ignored-we-are-already-setup))
 (lambda ()
   (setup-classpath-url-handler)))

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
(require-extension (srfi 14))
(require-extension (srfi 42)) ; Eager comprehensions.
(import siscweb/context)
(import siscweb/html)
(import siscweb/text)
(require-extension (lib iasylum/log))
(require-extension (lib iasylum/email))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/bot))
(require-extension (lib iasylum/excel))
(require-extension (lib iasylum/i18n))
(import networking)
(import binary-io)
(import custom-io)
(import file-manipulation)

;; and-let*. e.g.:
;; (and-let* ((v (assoc  "subscription_id" '(("subscription_id" ":db/unique    :db.unique/identity")) ))) (cdr v))
(require-extension (srfi 2))

(require-extension (lib iasylum/iasylum))

;; irregex
(require-library 'iasylum/irregex)

;; let-optionals
(require-library 'iasylum/let-optionals/let-optionals)

;; SLIB.
;;(load "iasylum/slib.scm")
(require-library 'iasylum/slib/iasylum-sisc.init)
;;(require 'new-catalog)

;; FIXXXME I am not sure with require 'line-i/o does not work, even after line-i/o
;; was added to supported features. This will do for now, providing the very useful
;; read-line.
(require-library 'iasylum/slib/3b2/lineio)

;(display "\n\nLOADED iasylum-bedlam.\n\n")

(begin
;; Jackcess and required libraries.
(add-lib "u/commons-lang3-3.4.jar")
(add-lib "microsoft_access/jackcess-1.2.12.jar")
)

(add-lib "u/junrar-0.7-SNAPSHOT.jar")

(add-lib "clojure/clojure-1.9.0-alpha15.jar")

(add-lib "clojure/tools.nrepl-0.2.10.jar")

(add-lib "u/pircbotx-2.1.jar")
(add-lib "clojure/core.async-0.1.0-SNAPSHOT.jar")
(add-lib "clojure/slack-rtm-0.1.7-standalone.jar") ;; It includes clj-slack 0.5.5


(define (extend-classpath i)
  (class-path-extension-append! (cons i (class-path-extension))))

(define (find-jars-in-directory dir)
  (map car (filter (lambda (v) (not (eqv? v '()))) (map (lambda (v) (irregex-extract (irregex '(seq (* any) ".jar")) v)) (rglob dir)))))

(define (find-scm-in-directory dir)
  (map car (filter (lambda (v) (not (eqv? v '()))) (map (lambda (v) (irregex-extract (irregex '(seq (* any) ".scm")) v)) (rglob dir)))))

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
    (require-library 'iasylum/fmt/fmt-0.8.1/let-optionals)  ; if you don't have LET-OPTIONALS*
    (require-library 'iasylum/fmt/fmt-0.8.1/read-line)      ; if you don't have READ-LINE
    (require-library 'iasylum/fmt/fmt-0.8.1/string-ports)   ; if you don't have CALL-WITH-OUTPUT-STRING
    (require-library 'iasylum/fmt/fmt-0.8.1/make-eq-table)
    (require-library 'iasylum/fmt/fmt-0.8.1/mantissa)
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt)
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-pretty)     ; optional pretty printing
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-column)     ; optional columnar output
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-c)          ; optional C formatting utilities
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-color)      ; optional color utilities
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-js)         ; javascript utilities. 
    (require-library 'iasylum/fmt/fmt-0.8.1/fmt-unicode))         ; javascript utilities.

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

      (if (and (eqv?  beanshell-httpd-port 3001) (eqv? beanshell-vanilla-port 3002)
               (eqv? clojure-nrepl-default-transport-port 6000) (eqv?  clojure-nrepl-tty-transport-port 6001))
          (begin (clojure/repl-start 6000)
                 (j "iu.M.i(siscport, 3001);" `((siscport ,(->jint sisc-repl-port))))

                 (string-append* "Starting repls at " sisc-repl-port "(SISC), 3001 (beanshell httpd), 3002 (beanshell), 6000 (clojure nrepl), 6001 (clojure tty transport)..." ))
          (error "nrepls: using any ports other than the default is not yet supported."))))

(define (start-bsh-service portnum)
  (j "		i = new bsh.Interpreter();
		i.set( \"data\", iu.M.d );
		i.set( \"portnum\", portnum );  
		i.eval(\"setAccessibility(true)\");
		i.eval(\"show()\");
		i.eval(\"server(portnum)\");"
     `((i)(portnum ,(->jint portnum)))))

	     
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
(import logicops)
(import string-io)
(import myenv)
(import util)
(import srfi-12)
(import srfi-13)
(import srfi-8)
(import parse-error)

(define native-read-string (unbox (box read-string)))
(define native-write-string write-string)
;(import input-parse)
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
(add-lib "u/json-io-2.7.2-SNAPSHOT-everything.jar")

(require-extension (lib iasylum/loop))
(require-extension (lib iasylum/math))
(require-extension (lib iasylum/javascript))
(require-extension (lib iasylum/hornetq))
(require-extension (lib iasylum/jmx))
(require-extension (lib iasylum/macros))

(add-lib "u/scrypt-1.4.0.jar")
(require-extension (lib iasylum/scrypt))

; Let's not hide stack traces by default, which making this easy to revert.
;(define default-suppressed-stack-trace-source-kinds (make-parameter (suppressed-stack-trace-source-kinds)))
;(suppressed-stack-trace-source-kinds '())

(define magic-load load)

(define (add-lib-recursively path . except)
  (map (lambda (full-path-file)
         (let ((full-path (->string full-path-file)))
           (and (not (member full-path except))
                (add-lib-fullpath full-path))))
       (->list (j "
                   public static java.util.Collection<String> extractAllPaths(java.util.Collection<String> currentList, java.io.File fileOrDir) throws java.io.IOException {
		    if (fileOrDir.exists()) {
			if (fileOrDir.isFile()) {
			    currentList.add(fileOrDir.getAbsolutePath());
			} else if (fileOrDir.isDirectory()) {
			    java.io.File[] files = fileOrDir.listFiles();
			    
			    if (files != null) {
				for (java.io.File file : files) {
				    extractAllPaths(currentList, file);
				}
			    }
			}
		    }
		    
		    return currentList;
		}
		
		public static java.util.Collection<String> extractAllPaths(String path) throws java.io.IOException {
		    return extractAllPaths(new java.util.ArrayList<String>(100), new java.io.File(path));
		}
		
		return extractAllPaths(input).toArray();
		"
                  `((input ,(->jstring path)))))))

(add-lib "net/commons-codec-1.6.jar")
(add-lib "net/fluent-hc-4.3.4.jar")
(add-lib "net/httpclient-cache-4.3.6.jar")
(add-lib "net/httpmime-4.3.4.jar")
(add-lib "net/httpcore-4.3.3.jar")
(add-lib "net/httpclient-4.3.6.jar")

(add-lib "jackson/jackson-databind-2.6.6.jar")
(add-lib "jackson/jackson-datatype-joda-2.6.6.jar")
(add-lib "jackson/jackson-core-2.6.6.jar")
(add-lib "jackson/jackson-annotations-2.6.6.jar")
(add-lib "jackson/jackson-core-asl-1.9.13.ja")

;; Incanter is a R-like environment for clojure that allows stuff similar to:
;; (clj "(use '(incanter core charts pdf)) (save-pdf (function-plot sin -4 4) \"./pdf-chart.pdf\")")
;; to be performed.
;; Split in two archives to stay below 100 mb.
(add-lib "u/incanter-1.5.6-SNAPSHOT-standalone-1_of_2.jar")
(add-lib "u/incanter-1.5.6-SNAPSHOT-standalone-2_of_2.jar")

(add-lib "u/clj-pdf-1.11.21-standalone.jar")

(require-extension (lib iasylum/net))
(require-extension (lib iasylum/debug)) ;; save-to-somewhere and other utilities.

(require-extension (lib iasylum/incanter))

(require-extension (lib iasylum/crypto))

(require-extension (lib iasylum/aws))

(add-lib "joda/joda-time-2.9.jar")

(add-lib "aws-java-sdk/javax-mail-1.4.6/javax.mail-api-1.4.6.jar")
(add-lib "aws-java-sdk/spring-3.0/spring-beans-3.0.7.jar")
(add-lib "aws-java-sdk/spring-3.0/spring-test-3.0.7.RELEASE.jar")
(add-lib "aws-java-sdk/spring-3.0/spring-core-3.0.7.jar")
(add-lib "aws-java-sdk/spring-3.0/spring-context-3.0.7.jar")
(add-lib "aws-java-sdk/aspectjrt-1.8.2.jar")
(add-lib "aws-java-sdk/aws-java-sdk-flow-build-tools-1.10.44.jar")
(add-lib "aws-java-sdk/aspectjweaver.jar")

(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_1.jar")
(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_2.jar")
(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_3.jar")
(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_4.jar")
(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_5.jar")
(add-lib "aws-java-sdk/aws-java-sdk-1.11.197_6.jar")


(add-lib "aws-java-sdk/freemarker-2.3.9.jar")

(require-extension (lib iasylum/compression))
(add-lib "u/lzstring4j.jar")

(add-lib "u/smtp-1.6.0.jar")

(add-lib "javascript/j2v8_linux_x86_64-4.8.0.jar")

(add-lib "u/bitcoinj-core-0.15-SNAPSHOT-bundled.jar")
(require-extension (lib iasylum/bitcoin))

(require-extension (lib iasylum/date))

(require-extension (lib iasylum/siscweb))

(require-extension (lib iasylum/csv))

(set! load original-load)

(define bedlam-loaded-and-ready-to-use #t)
