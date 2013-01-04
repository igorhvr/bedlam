(require-library 'ssax/ssax)

(module sxpath
  (SXPATH:sxpath)

  (import ssax)

  (include "sxpath-impl.scm")
  (require-library 'sxpath/sxpathlib)
  (require-library 'sxpath/sxpath-ext)
  (require-library 'sxpath/xpath-parser)
  (require-library 'sxpath/txpath)
  (require-library 'sxpath/sxpathlib)
  (require-library 'sxpath/sxml-tools))
