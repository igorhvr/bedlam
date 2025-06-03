(import custom-io)
(import string-io) ;; For call-with-output-string in tests
(import s2j)

;; Example usage: (define my-sink (lambda (p) (d/n "Uhuuul! " (iasylum-write-string p) " \n Bleug!\n\n"))) (r-base 'cmd-string: "head" (create-unary-function-based-output-port my-sink)  (mutex/new) (create-unary-function-based-output-port my-sink)  (mutex/new) (create-thunk-based-input-port (lambda () "SEGUNDO\n")) (mutex/new))
(module iasylum/function-ports
  (create-unary-function-based-output-port create-thunk-based-input-port)
  
  (include "function-ports-code.scm")

)
