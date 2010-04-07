;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

;(begin (define iasylum-bedlam-location "/home/igorhvr/idm/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))
(require-extension (srfi 23)) ; error

(with-failure-continuation 
 (lambda (e p)
   (display "\n\nThe variable iasylum-bedlam-location must be defined and initialized to the location of the library - which must be writable by you...\n")
   (display "\nExample of code to use this library.: \n")
   (display "   (begin (define iasylum-bedlam-location \"/home/igorhvr/idm/bedlam/\") (load (string-append iasylum-bedlam-location \"iasylum/init.scm\")))\n\n\n")
   
   
                                        ;(display "\n\nStarting iasylum-bedlam at [")
                                        ;(display iasylum-bedlam-location)
  ;(display "] ...\n\n")
  (throw e))
 (lambda ()
   (define tmp iasylum-bedlam-location)
   (string-append "test" tmp)
   (void)
   ))

(require-extension (srfi 39)) ; make-parameter

(define iasylum-bedlam-location
  (let ((param (make-parameter #f)))
    (param iasylum-bedlam-location)
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

(add-lib "u/jdbc-postgresql.jar")
(add-lib "excel/poi-3.2-FINAL-20081019.jar")
(add-lib "excel/poi-contrib-3.2-FINAL-20081019.jar")
(add-lib "excel/poi-scratchpad-3.2-FINAL-20081019.jar")
(add-lib "sisc/sisc-heap.jar")
(add-lib "sisc/sisc-lib.jar")
(add-lib "sisc/sisc-opt.jar")
(add-lib "sisc/sisc.jar")
(add-lib "siscweb/siscweb-sql.jar")
(add-lib "siscweb/siscweb-sxml.jar")
(add-lib "siscweb/siscweb.jar")
(add-lib "u/activation-1.1.jar")
(add-lib "u/commons-email-1.1.jar")
(add-lib "u/commons-fileupload-1.2.1.jar")
(add-lib "u/commons-io-1.4.jar")
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
(import networking) (import binary-io) (import custom-io)
(import threading) 
  ;;; IAsylum scheme files.
(class-path-extension-append! (cons (iasylum-bedlam-location) (class-path-extension)))

(max-stack-trace-depth 16)
(import debugging)
(require-extension (lib iasylum/jcode))

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
(require-extension (lib iasylum/iasylum))

;; irregex
(load (string-append (iasylum-bedlam-location) "iasylum/irregex.scm"))

;; let-optionals
(load (string-append (iasylum-bedlam-location) "iasylum/let-optionals/let-optionals.scm"))

;; SLIB
(load (string-append (iasylum-bedlam-location) "iasylum/slib.scm"))
(require 'new-catalog)

;(display "\n\nLOADED iasylum-bedlam.\n\n")
