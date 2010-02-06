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


;;; "rpn.scm" (RPN Calc) is an example of a functional-style program.
;;; As a result, the program is completely impervious to the back
;;; browser button and to browser window cloning.

(require-library 'sisc/libs/srfi/srfi-13) ; string library

(require-library 'siscweb/xhtml)
(require-library 'siscweb/bindings)

(module examples/rpn
  (rpn)

  (import srfi-13)

  (import siscweb/xhtml)
  (import siscweb/bindings)

  ;; returns the calculator page; this is the PRINT of REPL
  (define (rpn/print stack)
    (lambda (k-url)
      `(*TOP*
        (*PI* xml "version=\"1.0\"")
        (*DTD-INFO/PUBLIC* "html"
                           "-//W3C//DTD XHTML 1.0 Strict//EN"
                           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") (lang "en"))
         (head
          (title "RPN Calculator")
          (link (@ (rel "stylesheet")
                   (href-c "/css/default.css")
                   (type "text/css"))))
         (body (@ (onload "document.instructions.instructions.focus();"))
          (h2 "RPN Calculator")

          ;; prints the stack
          ,(if (null? stack)
               `(p "<empty stack>")
               `(ol
                 ,@(map (lambda (value) `(li (b ,(number->string value))))
                        (reverse stack))))

          (form (@ (action-e ,k-url) (name "instructions") (method "post"))
            (input (@ (id "instructions") (name "instructions") (type "text")))
            (input (@ (type "submit") (value "ENT"))))

          (p
           (label (@ (style "font-style: italic;"))
            "Enter any of <number> + - * / dup drop separated by spaces"))
          (p (a (@ (href-e ,k-url) (target "_blank")) "Clone me!"))
          (p (a (@ (href-c "/")) "^ Home")))))))



  ;; reads (parses) the input from the user; this is the READ in REPL
  (define (rpn/read stack)
    (let-bindings ((instructions "instructions"))
                  (get-bindings
                   (send-xhtml/suspend
                    (rpn/print stack)))
      (if instructions
          (string-tokenize instructions)
          '())))


  ;; interprets the instructions from the user; this is the EVAL in REPL
  (define (rpn/eval instructions stack)
    (let loop ((instructions instructions)
               (stack stack))
      (with/fc
       (lambda (m e) stack)
       (lambda ()
         (if (null? instructions)
             stack
             (let ((instr (car instructions)))
               (loop (cdr instructions)
                     (cond ((string->number instr)
                            (cons (string->number instr) stack))
                           ((string=? instr "drop")
                            (cdr stack))
                           ((string=? instr "dup")
                            (cons (car stack) stack))
                           (else (case (string->symbol instr)
                                   ((+ - * /)
                                    (apply-op (string->symbol instr) stack))
                                   (else stack)))))))))))

  ;; some snarfing because we're lazy
  (define (apply-op op stack)
    (cons (eval `(,op ,(cadr stack) ,(car stack)))
          (cddr stack)))


  ;; this is the main application loop; this is the Loop in REPL
  (define (rpn request)
    (let loop ((stack '(42)))
      (loop (rpn/eval (rpn/read stack) stack))))

  )