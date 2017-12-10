;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/srfi-89))

(module iasylum/random
  (random scheme-random random-maker
          
          secure-random-byte
          sort-of-secure-random-byte
          pseudo-random-uuid
          random-string
          random-var
          random->integer
          base58-bitcoin-like-random-code
          base26-random-code-upper
          random-gaussian)
  (import s2j)
  (include "random-code.scm")

  (random/init)
  )
