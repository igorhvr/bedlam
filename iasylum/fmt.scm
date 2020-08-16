;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

;;; This allows one to use the fmt formatter.

(require-extension (srfi 1 6 13 23 66 69))

(module iasylum/fmt
  (new-fmt-state
   fmt fmt-start fmt-if fmt-capture fmt-let fmt-bind fmt-null
   fmt-ref fmt-set! fmt-add-properties! fmt-set-property!
   fmt-col fmt-set-col! fmt-row fmt-set-row!
   fmt-radix fmt-set-radix! fmt-precision fmt-set-precision!
   fmt-properties fmt-set-properties! fmt-width fmt-set-width!
   fmt-writer fmt-set-writer! fmt-port fmt-set-port!
   fmt-decimal-sep fmt-set-decimal-sep!
   copy-fmt-state
   fmt-file fmt-try-fit cat apply-cat fmt/nl fl nl-str
   fmt-join fmt-join/last fmt-join/dot
   fmt-join/prefix fmt-join/suffix fmt-join/range
   pad pad/right pad/left pad/both trim trim/left trim/both trim/length
   fit fit/left fit/both tab-to space-to wrt wrt/unshared dsp
   pretty pretty/unshared slashified maybe-slashified
   num num/si num/fit num/comma radix fix decimal-align ellipses
   upcase downcase titlecase pad-char comma-char decimal-char
   with-width wrap-lines fold-lines justify
   make-string-fmt-transformer
   make-space make-nl-space display-to-string write-to-string
   fmt-columns columnar tabular line-numbers

   fmt-in-macro? fmt-expression? fmt-return? fmt-default-type
   fmt-newline-before-brace? fmt-braceless-bodies?
   fmt-indent-space fmt-switch-indent-space fmt-op fmt-gen
   c-in-expr c-in-stmt c-in-test
   c-paren c-maybe-paren c-type c-literal? c-literal char->c-char
   c-struct c-union c-class c-enum c-typedef c-cast
   c-expr c-expr/sexp c-apply c-op c-indent c-current-indent-string
   c-wrap-stmt c-open-brace c-close-brace
   c-block c-braced-block c-begin
   c-fun c-var c-prototype c-param c-param-list
   c-while c-for c-if c-switch
   c-case c-case/fallthrough c-default
   c-break c-continue c-return c-goto c-label
   c-static c-const c-extern c-volatile c-auto c-restrict c-inline
   c++ c-- c+ c- c* c/ c% c& c^ c~ c! c&& c<< c>> c== c!=
   c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= c^= c<<= c>>=
   c++/post c--/post c. c->
   c-bit-or c-or c-bit-or=
   cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
   cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
   cpp-error cpp-warning cpp-stringify cpp-sym-cat
   c-comment c-block-comment c-attribute

   js-expr js-function js-var js-comment js-array js-object js=== js>>>

   fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
   fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
   fmt-magenta fmt-black fmt-white fmt-bold fmt-underline
   
   unicode-char-width unicode-string-width fmt-unicode)
  

  (include "fmt/fmt-0.8.1/let-optionals.scm")  ; if you don't have LET-OPTIONALS*
  (include "fmt/fmt-0.8.1/read-line.scm")      ; if you don't have READ-LINE
  (include "fmt/fmt-0.8.1/string-ports.scm")   ; if you don't have CALL-WITH-OUTPUT-STRING
  (include "fmt/fmt-0.8.1/make-eq-table.scm")
  (include "fmt/fmt-0.8.1/mantissa.scm")
  (include "fmt/fmt-0.8.1/fmt.scm")
  (include "fmt/fmt-0.8.1/fmt-pretty.scm")     ; optional pretty printing
  (include "fmt/fmt-0.8.1/fmt-column.scm")     ; optional columnar output
  (include "fmt/fmt-0.8.1/fmt-c.scm")          ; optional C formatting utilities
  (include "fmt/fmt-0.8.1/fmt-color.scm")      ; optional color utilities
  (include "fmt/fmt-0.8.1/fmt-js.scm")         ; javascript utilities. 
  (include "fmt/fmt-0.8.1/fmt-unicode.scm")    ; optional Unicode-aware formatting,
                                                 ;   also requires SRFI-4 or SRFI-66
)
