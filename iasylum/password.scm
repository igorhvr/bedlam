; Port of plt scheme password package by Noel Welsh.
; file:///home/igorhvr/idm/scheme/implementations/plt/planet.plt-scheme.org/package-source/schematics/password.plt/1/0/password.ss
(require-extension (lib iasylum/random))
(require-extension (lib iasylum/memoize))

(require-extension (srfi 16))
(require-extension (srfi 13))

(module iasylum/password 
  (min-length
   max-length
   string->password
   make-password)
  (include "password-code.scm"))
