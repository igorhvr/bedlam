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

(module siscweb/request
  (current-request
   request/get-auth-type request/get-character-encoding
   request/get-content-length request/get-content-type
   request/get-context-path request/get-cookies
   request/get-date-header request/get-dispatcher request/get-header
   request/get-header-alist request/get-header-hashtable
   request/get-header-names request/get-int-header
   request/get-java-attribute request/get-java-attribute-names
   request/get-locale request/get-locales request/get-method
   request/get-parameter request/get-parameter-alist
   request/get-parameter-hashtable request/get-parameter-names
   request/get-parameter-values request/get-path-info
   request/get-path-translated request/get-protocol
   request/get-query-string request/get-remote-addr
   request/get-remote-host request/get-remote-user
   request/get-requested-session-id request/get-scheme
   request/get-server-name request/get-server-port
   request/get-servlet-path request/get-session request/get-uri
   request/get-url request/get-user-principal request/make-parameter
   request/open-binary-input-port request/open-input-port
   request/remove-java-attribute!
   request/requested-session-id-from-cookie?
   request/requested-session-id-from-url?
   request/requested-session-id-valid? request/secure?
   request/set-character-encoding! request/set-java-attribute!
   request/user-in-role?)

  (import s2j)
  (import hashtable)
  (import java-io)

  (import srfi-16)
  ;; time? is defined both in srfi-18 and srfi-19
  (import* srfi-18 make-mutex mutex-lock! mutex-unlock!)
  (import srfi-19)

  (import util/misc)


  (define-generic-java-methods
    (jentry-set |entrySet|)
    (jforward |forward|)
    (jget-attribute |getAttribute|)
    (jget-header |getHeader|)
    (jget-header-names |getHeaderNames|)
    (jget-key |getKey|)
    (jget-parameter-map |getParameterMap|)
    (jget-request-dispatcher |getRequestDispatcher|)
    (jget-request |getRequest|)
    (jget-session |getSession|)
    (jget-value |getValue|)
    (jiterator |iterator|)
    (jremove-attribute |removeAttribute|)
    (jset-attribute |setAttribute|)
    (to-string |toString|))


  (define (current-request)
    (jget-request (java-null (java-class '|siscweb.web.RequestScope|))))

  ;; a poor person's ffi generator
  ;; TODO: make this more generic?
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
           (->boolean (scheme-java-method (current-request))))))
      ((_ (boolean java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (->boolean (scheme-java-method (current-request) (->jstring string))))))
      ((_ (Date java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((ms (->number (scheme-java-method (current-request) (->jstring string)))))
             (if (= -1 ms)
                 #f
                 (make-time 'time-monotonic 0 (quotient ms 1000)))))))
      ((_ (Enumeration<Object> java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((enum (scheme-java-method (current-request))))
             (if (java-null? enum)
                 '()
                 (enumeration/map (lambda (o) o) enum))))))
      ((_ (int java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((n (->number (scheme-java-method (current-request)))))
             (if (= -1 n) #f n)))))
      ((_ (int java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((n (->number (scheme-java-method (current-request)
                                                  (->jstring string)))))
             (if (= -1 n) #f n)))))
      ((_ (Enumeration<String> java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((enum (scheme-java-method (current-request))))
             (if (java-null? enum)
                 '()
                 (enumeration/map ->string enum))))))
      ((_ (InputStream java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->binary-input-port (scheme-java-method (current-request))))))
      ((_ (Object java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (scheme-java-method (current-request)))))
      ((_ (ObjectArray java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((jarray (scheme-java-method (current-request))))
             (if (java-null? jarray)
                 '()
                 (->list jarray))))))
      ((_ (Reader java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->character-input-port (scheme-java-method (current-request))))))
      ((_ (String java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (let ((jstring (scheme-java-method (current-request))))
             (if (java-null? jstring) #f (->string jstring))))))
      ((_ (String java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((jstring (scheme-java-method (current-request) (->jstring string))))
             (if (java-null? jstring) #f (->string jstring))))))
      ((_ (StringArray java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (let ((jarray (scheme-java-method (current-request) (->jstring string))))
             (if (java-null? jarray)
                 '()
                 (map ->string (->list jarray)))))))
      ((_ (StringBuffer java-method))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda ()
           (->string (to-string (scheme-java-method (current-request)))))))
      ((_ (void java-method String))
       (let ((scheme-java-method (generic-java-method (quote java-method))))
         (lambda (string)
           (scheme-java-method (current-request) (->jstring string))
           (void))))))


  (define-syntax define-java-wrappers
    (syntax-rules ()
      ((_ (name signature-list) ...)
       (begin
         (define name
           (java-lambda signature-list)) ...))))

  (define-java-wrappers
    (request/get-java-attribute-names (Enumeration<String> |getAttributeNames|))
    (request/get-auth-type (String |getAuthType|))
    (request/get-character-encoding (String |getCharacterEncoding|))
    (request/get-content-length (int |getContentLength|))
    (request/get-content-type (String |getContentType|))
    (request/get-context-path (String |getContextPath|))
    (request/get-cookies (ObjectArray |getCookies|))
    (request/get-date-header (Date |getDateHeader| String))
    (request/get-header (String |getHeader| String))
    (request/get-header-names (Enumeration<String> |getHeaderNames|))
    (request/open-binary-input-port (InputStream |getInputStream|))
    (request/get-int-header (int |getIntHeader| String))
    (request/get-locale (Object |getLocale|))
    (request/get-locales (Enumeration<Object> |getLocales|))
    (request/get-method (String |getMethod|))
    (request/get-parameter (String |getParameter| String))
    (request/get-parameter-names (Enumeration<String> |getParameterNames|))
    (request/get-parameter-values (StringArray |getParameterValues| String))
    (request/get-path-info (String |getPathInfo|))
    (request/get-path-translated (String |getPathTranslated|))
    (request/get-protocol (String |getProtocol|))
    (request/get-query-string (String |getQueryString|))
    (request/open-input-port (Reader |getReader|))
    (request/get-remote-addr (String |getRemoteAddr|))
    (request/get-remote-host (String |getRemoteHost|))
    (request/get-remote-user (String |getRemoteUser|))
    (request/get-uri (String |getRequestURI|))
    (request/get-url (StringBuffer |getRequestURL|))
    (request/get-requested-session-id (String |getRequestedSessionId|))
    (request/get-scheme (String |getScheme|))
    (request/get-server-name (String |getServerName|))
    (request/get-servlet-path (String |getServletPath|))
    (request/get-server-port (int |getServerPort|))
    (request/get-user-principal (Object |getUserPrincipal|))
    (request/remove-attribute (void |removeAttribute| String))
    (request/requested-session-id-from-cookie? (boolean |isRequestedSessionIdFromCookie|))
    (request/requested-session-id-from-url? (boolean |isRequestedSessionIdFromURL|))
    (request/requested-session-id-valid? (boolean |isRequestedSessionIdValid|))
    (request/secure? (boolean |isSecure|))
    (request/set-character-encoding! (void |setCharacterEncoding| String))
    (request/user-in-role? (boolean |isUserInRole| String)))


  (define (request/get-scheme-attribute name)
    (let ((jvalue (request/get-java-attribute name)))
      (if (java-null? jvalue)
          #f
          (java-unwrap jvalue))))

  (define (request/set-scheme-attribute! name value)
    (request/set-java-attribute! name (if value (java-wrap value) jnull))
    (void))

  (define (request/remove-scheme-attribute! name)
    (request/remove-java-attribute! name))

  (define request/make-parameter
    (case-lambda
     ((name)
      (request/make-parameter name #f))
     ((name thread-safe?)
      (if thread-safe?
          (let ((mutex (make-mutex)))
            (case-lambda
             (()
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda () (request/get-scheme-attribute name))
               (lambda () (mutex-unlock! mutex))))
             ((value)
              (dynamic-wind
               (lambda () (mutex-lock! mutex))
               (lambda ()
                 (if value
                     (begin
                       (request/set-scheme-attribute! name value)
                       value)
                     (request/remove-scheme-attribute! name)))
               (lambda () (mutex-unlock! mutex))))))
          (case-lambda
           (() (request/get-scheme-attribute name))
           ((value)
            (if value
                (begin
                  (request/set-scheme-attribute! name value)
                  value)
                (request/remove-scheme-attribute! name))))))))


  (define (request/get-java-attribute name)
    (jget-attribute (current-request) (->jstring name)))

  (define (request/remove-java-attribute! name)
    (jremove-attribute (current-request) (->jstring name)))

  (define (request/set-java-attribute! name java-object)
    (jset-attribute (current-request) (->jstring name) java-object))


  (define (request/get-header-hashtable)
    (alist->hashtable (request/get-header-alist)))

  (define (request/get-header-alist)
    (enumeration/map
     (lambda (jname)
       (cons (->string jname)
             (->string (jget-header (current-request) jname))))
     (jget-header-names (current-request))))

  (define (request/get-parameter-hashtable)
    (alist->hashtable (request/get-parameter-alist)))

  (define (request/get-parameter-alist)
    (iterator/map
     (lambda (entry)
       (cons (->string (jget-key entry)) ; TODO: java methods
             (map ->string (->list (jget-value entry)))))
     (jiterator (jentry-set (jget-parameter-map (current-request))))))

  (define (request/get-dispatcher path)
    (let ((jdispatcher (jget-request-dispatcher (current-request) (->jstring path))))
      (if (java-null? jdispatcher)
          #f
          (lambda (request response)
            (jforward jdispatcher request response)))))


  (define request/get-session
    (case-lambda
      (() (request/get-session #t))
      ((create?) (jget-session (current-request) (->jboolean create?)))))
  )
