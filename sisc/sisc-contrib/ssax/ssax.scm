;; SISC module of SSAX code

;; These files define macros and I don't feel like messing with the
;; module system + macros yet.
(include "define-opt.scm")
(include "myenv-sisc.scm")

;; These define only functions
(load "look-for-str.scm")
(load "parser-error.scm")
(load "ascii.scm")
(load "input-parse.scm")
(load "util.scm")



(module ssax
  (xml-token?
   xml-token-kind xml-token-head
   make-empty-attlist attlist-add
   attlist-null?
   attlist-remove-top
   attlist->alist attlist-fold
   equal_?
   name-compare
   make-xml-token
   SSAX:largest-unres-name
   SSAX:read-pi-body-as-string
   SSAX:prefix-xml
   SSAX:resolve-name
   SSAX:warn
   SSAX:skip-internal-dtd
   SSAX:read-markup-token
   SSAX:read-CDATA-body
   SSAX:read-NCName
   SSAX:read-QName
   SSAX:read-char-ref
   SSAX:read-attributes
   SSAX:complete-start-tag
   SSAX:read-external-ID
   SSAX:read-char-data
   SSAX:make-pi-parser SSAX:make-elem-parser SSAX:make-parser
   SSAX:XML->SXML)

  (import parser-error)
  (import ascii)
  (import input-parse)
  (import util)
  (import miscio)

  (include "ssax-warn-vanilla.scm")
  (include "look-for-str.scm")
  (include "SSAX-code.scm")
  )