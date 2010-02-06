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
;;; Portions created by the Initial Developer are Copyright (C) 2005-2006
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

(require-library 'siscweb/contcentric)
(require-library 'siscweb/response)


(module siscweb/image
  (send-image/back send-image/finish
   send-image-file/back send-image-file/finish
   read-image write-image
   read-image-from-file write-image-to-file)

  (import s2j)
  (import binary-io)
  (import java-io)
  (import file-manipulation)

  (import srfi-16)

  (import siscweb/contcentric)
  (import siscweb/response)


  (define-java-classes
    (<misc-util> |siscweb.util.MiscUtil|))

  (define-generic-java-methods
    (jread-image |readImage|)
    (jwrite-image |writeImage|))


  (define send-image/back
    (case-lambda
      ((fmt image)
       (send-image/back '() fmt image))
      ((header-alst fmt image)
       (send/back
        (lambda ()
          (send-image header-alst fmt image))))))

  (define send-image/finish
    (case-lambda
      ((fmt image)
       (send-image/finish '() fmt image))
      ((header-alst fmt image)
       (send/finish
        (lambda ()
          (send-image header-alst fmt image))))))


  (define send-image-file/back
    (case-lambda
      ((fmt url)
       (send-image-file/back '() fmt url))
      ((header-alst fmt url)
       (send/back
        (lambda ()
          (send-image-file header-alst fmt url))))))

  (define (send-image-file/finish fmt url)
    (case-lambda
      ((fmt url)
       (send-image-file/finish '() fmt url))
      ((header-alst fmt url)
       (send/finish
        (lambda ()
          (send-image-file header-alst fmt url))))))


  (define (send-image header-alst fmt image)
    (when (not (assoc "Content-Type" header-alst))
      (response/set-content-type! (make-mime-type fmt)))
    (response/add-headers! header-alst)
    (write-image fmt image (response/open-binary-output-port)))


  (define (send-image-file header-alst fmt url)
    (when (not (assoc "Content-Type" header-alst))
      (response/set-content-type! (make-mime-type fmt)))
    (when (not (assoc "Content-Length" header-alst))
      (response/set-content-length! (file-length url)))
    (call-with-binary-input-file
     url
     (lambda (in-port)
       (copy-binary in-port (response/open-binary-output-port) (response/get-buffer-size)))))


  (define write-image
    (case-lambda
      ((fmt image)
       (write-image fmt image (current-output-port)))
      ((fmt image out-port)
       (jwrite-image (java-null <misc-util>)
                     (->jstring fmt)
                     image
                     (->joutput-stream out-port)))))

  (define read-image
    (case-lambda
      ((fmt)
       (read-image fmt (current-input-port)))
      ((fmt in-port)
       (jread-image (java-null <misc-util>)
                    (->jstring fmt)
                    (->jinput-stream in-port)))))

  (define (read-image-from-file fmt url)
    (call-with-binary-input-file
     url
     (lambda (in-port)
       (read-image fmt in-port))))

  (define (write-image-to-file fmt image url)
    (call-with-binary-output-file
     url
     (lambda (out-port)
       (write-image fmt image out-port)))
    (void))


  (define (make-mime-type fmt)
    (format "image/~a" fmt))

  (define (copy-binary in out bufsize)
    (define (C buf)
      (define (CB)
        (let ((cnt (read-block buf 0 bufsize in)))
          (when (not (eof-object? cnt))
            (write-block buf 0 cnt out)
            (CB))))
      (CB))
    (C (make-buffer bufsize)))

  )
