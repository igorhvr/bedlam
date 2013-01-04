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


(require-library 'sisc/libs/srfi/srfi-1) ; list library
(require-library 'sisc/libs/srfi/srfi-16) ; syntax for procedures of variable arity

(require-library 'siscweb/contcentric)
(require-library 'siscweb/response)
(require-library 'sxml/dotml)
(require-library 'util/misc)


(module siscweb/graphviz
  (send-graphviz/back send-graphviz/finish
   graphviz/put-layout-command! graphviz/get-layout-command
   graphviz/put-mime-type! graphviz/get-mime-type graphviz/binary-format?
   graphviz/write-dot graphviz/write)

  (import buffers)
  (import hashtable)
  (import oo)
  (import os)
  (import string-io)
  (import threading)
  (import type-system)

  (import srfi-1)
  (import srfi-16)

  (import siscweb/bindings)
  (import siscweb/contcentric)
  (import siscweb/response)
  (import sxml/dotml)
  (import util/misc)


  (define graphviz/write-dot
    (case-lambda
      ((layout fmt graph)
       (graphviz/write-dot layout fmt graph (current-output-port)))
      ((layout fmt graph port)
       (let ((layout-command (graphviz/get-layout-command layout))
             (exists-format? (graphviz/get-mime-type fmt)))
         (when (not layout-command)
           (error (format "siscweb/graphviz: Unknown layout : \"~a\"" layout)))
         (when (not exists-format?)
           (error (format "siscweb/graphviz: Unknown format : \"~a\"" fmt)))
         (run layout-command (list (format "-T~a" fmt)) graph port)))))


  (define (run program options input-string out)
    (let* ((ps (spawn-process program options))
           (ps-in (open-character-output-port
                   (get-process-stdin ps)))
           (ps-out (get-process-stdout ps))
           (ps-err (get-process-stderr ps)))
      ;; 1. feeds input string into process stdin
      (thread/spawn
       (lambda ()
         (dynamic-wind
             (lambda () #f)
             (lambda () (display input-string ps-in))
             (lambda () (close-output-port ps-in)))))

      ;; 2. copies process stdout to out
      (if (binary-output-port? out)
          (copy-binary ps-out out)
          (copy ps-out out))
      (flush-output-port out)

      ;; 3. gets return code and error if nonzero (this may be a bit
      ;; extreme)
      (let ((rc (wait-for-process ps)))
         (if (= 0 rc)
             rc
             (error
              (with-output-to-string
                (lambda ()
                  (copy ps-err out))))))))


  (define graphviz/write
    (case-lambda
      ((layout fmt dotml)
       (graphviz/write layout fmt dotml (current-output-port)))
      ((layout fmt dotml port)
       (graphviz/write-dot layout fmt (dotml->dot dotml) port))))

  (define (graphviz/send layout fmt dotml)
    (response/set-content-type! (graphviz/get-mime-type fmt))
    (let ((out (if (graphviz/binary-format? fmt)
                   (response/open-binary-output-port)
                   (response/open-output-port))))
      (graphviz/write layout fmt dotml out)))

  (define (send-graphviz/back layout fmt dotml)
    (send/back
     (lambda ()
       (graphviz/send layout fmt dotml))))

  (define (send-graphviz/finish layout fmt dotml)
    (send/finish
     (lambda ()
       (graphviz/send layout fmt dotml))))


  ;; get/set associations between layout and commands
  (define (graphviz/put-layout-command! layout command)
    (hashtable/put! layout-table layout command))

  (define (graphviz/get-layout-command layout)
    (hashtable/get layout-table layout))

  ;; mime map table
  (define (graphviz/put-mime-type! fmt mime-type binary?)
    (hashtable/put! mime-type-table fmt (list mime-type binary?)))

  (define (graphviz/get-mime-type fmt)
    (let ((entry (hashtable/get mime-type-table fmt)))
      (if entry (car entry) #f)))

  ;; binary format table
  (define (graphviz/binary-format? fmt)
    (let ((entry (hashtable/get mime-type-table fmt)))
      (if entry (cadr entry) #f)))

  ;; I/O copy
  (define (copy-binary in out)
    (define (C buf)
      (define (CB)
        (let ((cnt (read-block buf 0 5000 in)))
          (when (not (eof-object? cnt))
            (write-block buf 0 cnt out)
            (CB))))
      (CB))
    (let ((buf (make-buffer 5000)))
      (C buf)))

  (define (copy in out)
    (define (C)
      (let ((chr (read-char in)))
        (when (not (eof-object? chr))
          (write-char chr out)
          (C))))

    (when (binary-input-port? in)
      (set! in (open-character-input-port in)))
    (C))

  ;; tables
  (define layout-table
    (alist->hashtable
     `(("dot" . "dot")
       ("neato" . "neato")
       ("twopi" . "twopi")
       ("circo" . "circo")
       ("fdp" . "fdp"))
     equal? #f))

  (define mime-type-table
    (alist->hashtable
     '(("cmapx" "text/html" #f)
       ("fig" "application/x-xfig" #t)
       ("gif" "image/gif" #t)
       ("hpgl" "application/vnd.hp-HPGL" #t)
       ("jpg" "image/jpeg" #t)
       ("mif" "application/vnd.mif" #f)
       ("pcl" "application/vnd.hp-PCL" #t)
       ("png" "image/png" #t)
       ("ps" "application/postscript" #f)
       ("svg" "image/svg+xml" #f))
     equal? #f))
  )
