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
(require-library 'sisc/libs/srfi/srfi-19) ; time data types and procedures

(require-library 'util/misc)

(module siscweb/response
  (current-response
   response/add-cookie! response/add-header! response/add-headers!
   response/commit! response/committed? response/contains-header?
   response/encode-redirect-url response/encode-url
   response/get-buffer-size response/get-character-encoding
   response/get-locale response/open-binary-output-port
   response/open-output-port response/reset!  response/reset-buffer!
   response/send-error response/send-redirect
   response/set-buffer-size! response/set-content-length!
   response/set-content-type! response/set-header!
   response/set-locale! response/set-status!)

  (import s2j)
  (import java-io)

  (import srfi-16)
  (import srfi-19)

  (import util/misc)

  (define-generic-java-methods
    (jadd-cookie |addCookie|)
    (jadd-date-header |addDateHeader|)
    (jadd-header |addHeader|)
    (jadd-int-header |addIntHeader|)
    (jcontains-header |containsHeader|)
    (jencode-redirect-url |encodeRedirectURL|)
    (jencode-url |encodeURL|)
    (jflush-buffer |flushBuffer|)
    (jget-buffer-size |getBufferSize|)
    (jget-character-encoding |getCharacterEncoding|)
    (jget-locale |getLocale|)
    (jget-output-stream |getOutputStream|)
    (jget-response |getResponse|)
    (jget-writer |getWriter|)
    (jis-committed |isCommitted|)
    (jreset |reset|)
    (jreset-buffer |resetBuffer|)
    (jsend-error |sendError|)
    (jsend-redirect |sendRedirect|)
    (jset-buffer-size |setBufferSize|)
    (jset-content-length |setContentLength|)
    (jset-content-type |setContentType|)
    (jset-date-header |setDateHeader|)
    (jset-header |setHeader|)
    (jset-int-header |setIntHeader|)
    (jset-locale |setLocale|)
    (jset-status |setStatus|))


  (define (current-response)
    (jget-response (java-null (java-class '|siscweb.web.RequestScope|))))

  (define (response/add-cookie! cookie)
    (jadd-cookie (current-response) cookie)
    (void))

  (define (response/add-header! key value)
    (cond ((number? value)
           (jadd-int-header (current-response) (->jstring key) (->jint value)))
          ((date? value)
           (response/add-header! key (date->time-utc value)))
          ((time? value)
           (jadd-date-header (current-response) (->jstring key)
                            (->jlong (* 1000 (time-second value)))))
          (else
           (jadd-header (current-response) (->jstring key) (->jstring value))))
    (void))

  (define (response/add-headers! alst)
    (for-each
     (lambda (pair)
       (let ((key (car pair))
             (value (cadr pair)))
         (if (equal? key "Content-Type")
             (response/set-content-type! value)
             (response/add-header! key value))))
     alst)
    (void))

  (define (response/contains-header? key)
    (->boolean (jcontains-header (current-response) (->jstring key))))

  (define (response/encode-redirect-url url)
    (->string (jencode-redirect-url (current-response) (->jstring url))))

  (define (response/encode-url url)
    (->string (jencode-url (current-response) (->jstring url))))

  (define (response/commit!)
    (jflush-buffer (current-response))
    (void))

  (define (response/get-buffer-size)
    (->number (jget-buffer-size (current-response))))

  (define (response/get-character-encoding)
    (->string (jget-character-encoding (current-response))))

  (define (response/get-locale)
    (jget-locale (current-response)))

  ;; TODO: rename to response/open-*-output-port
  (define (response/open-binary-output-port)
    (->binary-output-port (jget-output-stream (current-response)) #f))

  (define (response/open-output-port)
    (->character-output-port (jget-writer (current-response)) #f))

  (define (response/committed?)
    (->boolean (jis-committed (current-response))))

  (define (response/reset!)
    (jreset (current-response)))

  (define (response/reset-buffer!)
    (jreset-buffer (current-response)))

  (define response/send-error
    (case-lambda
      ((sc)
       (jsend-error (current-response) (->jint sc))
       (void))
      ((sc msg)
       (jsend-error (current-response) (->jint sc) (->jstring msg))
       (void))))


  (define (response/send-redirect url)
    (jsend-redirect (current-response) (->jstring url))
    (void))

  (define (response/set-buffer-size! size)
    (jset-buffer-size (current-response) (->jint size))
    (void))

  (define (response/set-content-length! len)
    (jset-content-length (current-response) (->jint len)))

  (define (response/set-content-type! mime-type)
    (jset-content-type (current-response) (->jstring mime-type))
    (void))

  (define (response/set-header! key value)
    (cond ((number? value)
           (jset-int-header (current-response) (->jstring key) (->jint value)))
          ((date? value)
           (response/set-header! key (date->time-utc value)))
          ((time? value)
           (jset-date-header (current-response) (->jstring key) (->jlong (* 1000 (time-second value)))))
          (else
           (jset-header (current-response) (->jstring key) (->jstring value))))
    (void))

  (define (response/set-locale! loc)
    (jset-locale (current-response) loc)
    (void))

  (define (response/set-status! sc)
    (jset-status (current-response) (->jint sc)))

  )
