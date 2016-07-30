;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/srfi-89))

(module iasylum/random
  (random scheme-random random-maker)
  (import s2j)
  (include "random-code.scm")
  )
