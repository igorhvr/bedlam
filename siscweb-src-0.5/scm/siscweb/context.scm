;;; The contents of this file are subject to the Mozilla Public License Version
;;; 1.1 (the "License"); you may not use this file except in compliance with
;;; the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS" basis,
;;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;; for the specific language governing rights and limitations under the
;;; License.
;;;
;;; The Original Code is SISCweb.
;;;
;;; The Initial Developer of the Original Code is Alessandro Colomba.
;;; Portions created by the Initial Developer are Copyright (C) 2005-2007
;;; Alessandro Colomba. All Rights Reserved.
;;;
;;; Contributor(s):
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either the GNU General Public License Version 2 or later (the "GPL"), or
;;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;;; in which case the provisions of the GPL or the LGPL are applicable instead
;;; of those above. If you wish to allow use of your version of this file only
;;; under the terms of either the GPL or the LGPL, and not to allow others to
;;; use your version of this file under the terms of the MPL, indicate your
;;; decision by deleting the provisions above and replace them with the notice
;;; and other provisions required by the GPL or the LGPL. If you do not delete
;;; the provisions above, a recipient may use your version of this file under
;;; the terms of any one of the MPL, the GPL or the LGPL.


(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity
(require-library 'sisc/libs/srfi/srfi-18) ; multithreading support
(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures

(require-library 'util/misc)

(module siscweb/context
  (current-context context/get context/get-dispatcher
   context/get-init-parameter context/get-init-parameter-alist
   context/get-init-parameter-hashtable
   context/get-init-parameter-names context/get-java-attribute
   context/get-java-attribute-names context/get-major-version
   context/get-mime-type context/get-minor-version context/get-name
   context/get-named-dispatcher context/get-real-path
   context/get-resource context/get-resource-paths
   context/get-server-info context/log context/make-parameter
   context/open-resource-binary-input-port
   context/remove-java-attribute! context/set-java-attribute!)

  (import s2j)
  (import hashtable)
  (import java-io)

  (import srfi-16)
  ;; time? is defined both in srfi-18 and srfi-19
  (import* srfi-18 make-mutex mutex-lock! mutex-unlock!)
  (import srfi-19)

  (import util/misc)

  (define-generic-java-methods
    (jforward |forward|)
    (jget-attribute |getAttribute|)
    (jget-attribute-names |getAttributeNames|)
    (jget-context |getContext|)
    (jget-init-parameter |getInitParameter|)
    (jget-init-parameter-names |getInitparameterNames|)
    (jget-named-dispatcher |getNamedDispatcher|)
    (jget-request-dispatcher |getRequestDispatcher|)
    (jget-resource |getResource|)
    (jget-resource-as-stream |getResourceAsStream|)
    (jlog |log|)
    (jremove-attribute |removeAttribute|)
    (jset-attribute |setAttribute|)
    (jset-init-parameter |setInitParameter|)
    (jto-string |toString|))


  (define-syntax java-lambda
    (syntax-rules (boolean int
                   Date
                   Enumeration<Object> Enumeration<String>
                   InputStream
                   Object ObjectArray
                   Reader
                   String StringArray StringBuffer
                   void)
      ((_ (boolean java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->boolean (scheme-java-method (current-context))))))
      ((_ (boolean java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (->boolean (scheme-java-method (current-context) (->jstring string))))))
      ((_ (Date java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((ms (->number (scheme-java-method (current-context) (->jstring string)))))
             (if (= -1 ms)
                 #f
                 (make-time 'time-monotonic 0 (quotient ms 1000)))))))
      ((_ (Enumeration<Object> java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((enum (scheme-java-method (current-context))))
             (if (java-null? enum)
                 '()
                 (enumeration/map (lambda (o) o) enum))))))
      ((_ (int java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((n (->number (scheme-java-method (current-context)))))
             (if (= -1 n) #f n)))))
      ((_ (int java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((n (->number (scheme-java-method (current-context)
                                                  (->jstring string)))))
             (if (= -1 n) #f n)))))
      ((_ (Enumeration<String> java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((enum (scheme-java-method (current-context))))
             (if (java-null? enum)
                 '()
                 (enumeration/map ->string enum))))))
      ((_ (InputStream java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->binary-input-port (scheme-java-method (current-context))))))
      ((_ (Object java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (scheme-java-method (current-context)))))
      ((_ (ObjectArray java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((jarray (scheme-java-method (current-context))))
             (if (java-null? jarray)
                 '()
                 (->list jarray))))))
      ((_ (Reader java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->character-input-port (scheme-java-method (current-context))))))
      ((_ (String java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((jstring (scheme-java-method (current-context))))
             (if (java-null? jstring) #f (->string jstring))))))
      ((_ (String java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((jstring (scheme-java-method (current-context) (->jstring string))))
             (if (java-null? jstring) #f (->string jstring))))))
      ((_ (StringArray java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((jarray (scheme-java-method (current-context) (->jstring string))))
             (if (java-null? jarray)
                 '()
                 (map ->string (->list jarray)))))))
      ((_ (StringBuffer java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->string (to-string (scheme-java-method (current-context)))))))
      ((_ (void java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (scheme-java-method (current-context) (->jstring string))
           (void))))))

  ;; DO NOT modify this; it's a copy
  (define-syntax define-java-wrappers
    (syntax-rules ()
      ((_ (name signature-list) ...)
       (begin
         (define name
           (java-lambda signature-list)) ...))))


  (define-java-wrappers
    (context/get-init-parameter (String |getInitParameter| String))
    (context/get-init-parameter-names (Enumeration<String> |getInitParameterNames|))
    (context/get-major-version (int |getMajorVersion|))
    (context/get-mime-type (String |getMimeType| String))
    (context/get-minor-version (int |getMinorVersion|))
    (context/get-real-path (String |getRealPath| String))
    (context/get-server-info (String |getServerInfo|))
    (context/get-name (String |getServletContextName|)))


  (define (context/get-init-parameter-hashtable)
    (alist->hashtable (context/get-init-parameter-alist)))

  (define (context/get-init-parameter-alist)
    (enumeration/map
     (lambda (jname)
       (let ((jvalue (jget-init-parameter (current-context) jname)))
         (when (not (java-null? jvalue)) ; concurrency safeguard
           (cons (->string jname) (->jstring jvalue)))))
     (jget-init-parameter-names (current-context))))


  (define (context/get-scheme-attribute name)
    (let ((jvalue (context/get-java-attribute name)))
      (if (java-null? jvalue)
          #f
          (java-unwrap jvalue))))

  (define (context/set-scheme-attribute! name value)
    (context/set-java-attribute! name (if value (java-wrap value) jnull))
    (void))

  (define (context/remove-scheme-attribute! name)
    (context/remove-java-attribute! name))

  (define context/make-parameter
    (case-lambda
     ((name)
      (context/make-parameter name #f))
     ((name thread-safe?)
      (if thread-safe?
          (let ((mutex (make-mutex)))
            (case-lambda
             (()
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda () (context/get-scheme-attribute name))
               (lambda () (mutex-unlock! mutex))))
             ((value)
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda ()
                 (if value
                     (begin
                       (context/set-scheme-attribute! name value)
                       value)
                     (context/remove-scheme-attribute! name)))
               (lambda () (mutex-unlock! mutex))))))
          (case-lambda
           (() (context/get-scheme-attribute name))
           ((value)
            (if value
                (begin
                  (context/set-scheme-attribute! name value)
                  value)
                (context/remove-scheme-attribute! name))))))))

  (define (context/get-java-attribute name)
    (jget-attribute (current-context) (->jstring name)))

  (define (context/remove-java-attribute! name)
    (jremove-attribute (current-context) (->jstring name))
    (void))

  (define (context/set-java-attribute! name java-object)
    (jset-attribute (current-context) (->jstring name) java-object)
    (void))

  (define (context/get-java-attribute-names)
    (enumeration/map
     ->string
     (jget-attribute-names (current-context))))


  (define (context/get uripath)
    (jget-context (current-context) (->jstring uripath)))

  (define (context/get-named-dispatcher name)
    (let ((jdispatcher (jget-named-dispatcher (current-context) (->jstring name))))
      (if (java-null? jdispatcher)
          #f
          (lambda (request response)
            (jforward jdispatcher request response)))))

  (define (context/get-dispatcher path)
    (let ((jdispatcher (jget-request-dispatcher (current-context) (->jstring path))))
      (if (java-null? jdispatcher)
          #f
          (lambda (request response)
            (jforward jdispatcher request response)))))

  (define (context/get-resource path)
    (let ((jurl (jget-resource (current-context) (->jstring path))))
      (if (java-null? jurl)
          #f
          (->string (jto-string jurl)))))

  (define (context/open-resource-binary-input-port path)
    (let ((jis (jget-resource-as-stream (current-context) (->jstring path))))
      (if (java-null? jis)
          #f
          (->binary-input-port jis))))

  (define (context/get-resource-paths)
    #f) ; TODO: implement

  (define context/log
    (case-lambda
      ((msg) (jlog (current-context) (->jstring msg)) (void))
      ((msg throwable) (jlog (current-context) (->jstring msg) throwable) (void))))



  (define (current-context)
    *SISCWEB.SERVLET-CONTEXT*)
  )
