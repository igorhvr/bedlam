(add-lib "u/guava-19.0.jar")

(require-extension (lib iasylum/jcode))

(module iasylum/guava
  (make-cache
   cache-put!
   cache-get
   )

  (import iasylum/iasylum)
  (include "guava-code.scm")
  )

