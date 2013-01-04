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
;;; Portions created by the Initial Developer are Copyright (C) 2005
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


(require-library 'util/misc)

(module sql/jdbc
  (jdbc/call-with-connection jdbc/call/conn
   jdbc/call-with-transaction jdbc/call/tran
   jdbc/close-connection jdbc/get-connection jdbc/open-connection
   jdbc/load-driver
   jdbc/get-vendor)

  (import s2j)
  (import generic-procedures)

  (import util/misc)


  (define-java-classes
    (<class> |java.lang.Class|)
    (<connection> |java.sql.Connection|)
    (<data-source> |javax.sql.DataSource|)
    (<driver-manager> |java.sql.DriverManager|)
    (<system> |java.lang.System|))

  (define-generic-java-field-accessor :out)

  (define-generic-java-methods close commit get-auto-commit (jget-connection |getConnection|) for-name get-database-product-name get-meta-data rollback set-auto-commit to-lower-case)


  (define (jdbc/load-driver name)
    (with/fc (lambda (m e) #f)
             (lambda ()
               (for-name (java-null <class>)
                         (->jstring name))
               #t)))

  ;; TODO: test&fix
  (define-java-proxy (jdbc/make-data-source url username password)
    (<data-source>)
    (define (get-connection)
      (jdbc/open-connection url username password))
    (define (get-connection usr pwd)
      (jdbc/open-connection url usr pwd))
    (define (get-login-timeout)
      (->jint 0))
    (define (get-log-writer)
      (:out (java-null <system>)))
    (define (set-login-timeout seconds)
      #!void)
    (define (set-log-writer out)
      #!void))

  (define (jdbc/get-connection ds)
    (jget-connection ds))

  (define (jdbc/open-connection url username password)
    (jget-connection (java-null <driver-manager>)
                     (->jstring url)
                     (->jstring username)
                     (->jstring password)))

  (define (jdbc/close-connection conn)
    (close conn))

  (define (jdbc/call-with-connection conn proc)
    (dynamic-wind
        (lambda () #f)
        (lambda () (proc conn))
        (lambda () (jdbc/close-connection conn))))

  (define jdbc/call/conn jdbc/call-with-connection)

  ;; TODO: add transaction isolation and extent (and rollback)
  (define (jdbc/call-with-transaction conn proc)
    (let ((jac (get-auto-commit conn)))
      (dynamic-wind
          (lambda ()
            (set-auto-commit conn (->jboolean #f)))
          (lambda ()
            (with/fc
             (lambda (m e)
               (rollback conn)
               (throw m e))
             (lambda ()
               (let ((result (proc conn)))
                 (commit conn)
                 result))))
          (lambda ()
            (set-auto-commit conn jac)))))

  (define jdbc/call/tran jdbc/call-with-transaction)


  (define (jdbc/get-vendor conn)
    (->symbol (to-lower-case (get-database-product-name (get-meta-data conn)))))
  )
