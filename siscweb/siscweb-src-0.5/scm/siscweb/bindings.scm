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

(require-library 'util/srfi-71)
(require-library 'util/misc)

(module siscweb/bindings
  (get-bindings
   exists-binding? extract-single-binding extract-bindings
   alist->bindings bindings->alist
   hashtable->bindings bindings->hashtable
   let-bindings
   bindings/get bindings/exists? bindings/extract bindings/extract-single
   (bindings/let srfi-let bindings/extract))

  (import s2j)
  (import hashtable)
  (import type-system)

  (import srfi-16)

  (import util/srfi-71)
  (import util/misc)

  (define-java-classes
    (<binding-list> |siscweb.util.BindingList|)
    (<jstring> |java.lang.String|)
    (<jobject> |java.lang.Object|)
    (<jobject-array> |java.lang.Object[]|)
    (<map> |java.util.Map|)
    (<hash-map> |java.util.HashMap|)
    (<http-servlet-request> |javax.servlet.http.HttpServletRequest|)
    (<sisc-http-servlet-request> |siscweb.web.SISCHttpServletRequest|))

  (define-generic-java-methods add contains-key get
    get-binding-map get-parameter get-parameter-map put
    set-character-encoding size)


  (define get-bindings
    (case-lambda
      ((char-enc request)
       (set-character-encoding request (->jstring char-enc))
       (get-bindings request))
      ((request)
       (if (instance-of? request <sisc-http-servlet-request>)
           (get-binding-map request)
           (java-new <hash-map> (get-parameter-map request))))))


  (define (exists-binding? name bindings)
    (->boolean (contains-key bindings (->jstring name))))


  (define (extract-single-binding name bindings)
    (let ((binding (extract-binding name bindings)))
      (cond ((java-null? binding)
             #f)
            ((instance-of? binding <jobject-array>)
             (and (> (java-array-length binding) 0)
                  (b->s (java-array-ref binding 0))))
            ((instance-of? binding <binding-list>)
             (and (> (->number (size binding)) 0)
                  (b->s (get binding (->jint 0)))))
            (else
             (b->s binding)))))

  (define (extract-bindings name bindings)
    (binding->scheme (extract-binding name bindings)))


  (define (alist->bindings alist)
    (define (list->binding-list lst)
      (let ((bl (java-new <binding-list>)))
        (for-each
         (lambda (x)
           (add bl (s->b x)))
         lst)
        bl))

    (let ((bindings (java-new <hash-map> (->jint (length alist)))))
      (for-each
       (lambda (pair)
         (let ((name (car pair))
               (value (cdr pair)))
           (put bindings
                (->jstring name)
                (cond ((not value)
                       (java-null <jobject>))
                      ((list? value)
                       (list->binding-list value))
                      (else
                       (s->b value))))))
       alist)
      bindings))


  (define (bindings->alist bindings)
    (jmap/map
     (lambda (jkey jvalue)
       (cons (->string jkey)
             (binding->scheme jvalue)))
     bindings))

  (define (hashtable->bindings ht)
    (alist->bindings (hashtable->alist ht)))

  (define (bindings->hashtable bindings)
    (alist->hashtable (bindings->alist bindings) equal?))



  ;; binding getter
  ;; returns : - an empty list if the binding does not exist
  ;;           - an Object[], BindingList or Java value
  ;; in the latter case no conversion is made because it would be
  ;; wasteful to convert multiple value when the return value is
  ;; used by extract-single-binding, which only uses one
  (define (extract-binding name bindings)
    (get bindings (->jstring name)))
;;        (error (format "extract-binding: expecting <bindings> object or a-list, found : ~a" (type-of bindings)))))))


  ;; converts an Object[], BindingList or Java value
  ;; to a Scheme value
  (define (binding->scheme binding)
    (cond ((java-null? binding)
           '())
          ((instance-of? binding <jobject-array>)
           (jarray/map b->s binding))
          ((instance-of? binding <binding-list>)
           (jlist/map b->s binding))
          (else
           (b->s binding))))

  ;; converts a single binding value to Scheme
  (define (b->s value)
    (typecase value
      ((<jstring>) (->string value))   ; jstrings are converted
      ((<jvalue>) (java-unwrap value)) ; wrapped scheme type
      ((<jobject>) value)))            ; java objects are left alone

  ;; converts a single scheme value to Java
  (define (s->b value)
    (typecase value
      ((<string>) (->jstring value))  ; scheme strings are converted
      ((<jobject>) value)             ; java objects are left alone
      ((<value>) (java-wrap value)))) ; scheme values are wrapped



  ;; a convenience form to assign bindings to variables
  ;; var is the scheme variable name, name is the binding name
  (define-syntax let-bindings
    (syntax-rules (single multiple) ;for future use
      ((_ ((var name) ...) bindings body ...)
       (let* ((bnd bindings)
              (var (extract-single-binding name bnd))
              ... )
         body ...))))


  ;; NEW API

  (define bindings/get get-bindings)

  (define (bindings/exists? name bindings)
    (exists-binding name bindings))

  (define (bindings/extract name bindings)
    (unlist (extract-bindings name bindings)))

  (define (bindings/extract-single name bindings)
    (extract-single-binding name bindings))

  (define-syntax bindings/let
    (syntax-rules (single multiple) ;for future use
      ((_ ((var ... name) ...) bindings body ...)
       (let ((bnd bindings))
         (srfi-let ((var ... (bindings/extract name bnd))
                    ... )
           body ...)))))
  )
