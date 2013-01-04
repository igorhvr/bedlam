(require-library 'sisc/libs/srfi)

(require-library 'siscweb/bindings)
(require-library 'siscweb/config)
(require-library 'siscweb/forward)
(require-library 'siscweb/response)

(require-library 'lshift/error)

(require-library 'util/misc)
(require-library 'util/regexp)

(import file-manipulation)

(define dest "../bin/")

(current-directory "../scm")

(for-each
 (lambda (name)
   (make-directory! (string-append dest name)))
 '("siscweb" "lshift" "ssax" "sxml" "sxpath" "webit"))

(for-each
 (lambda (name)
   (display (string-append "Compiling " name " ..."))
   (compile-file (string-append name ".scm")
                 (string-append dest name ".scc"))
   (display "done.\n"))
 '("webit/keyword" "webit/xml-core"
   "sxml/sxml-match" "sxml/markup" "sxml/dotml"
   "siscweb/graphviz" "sxml/ehtml" "sxml/sxml-to-xml"
   "sxml/sxml-to-html" "sxml/sxml-tree-trans"
   "siscweb/xhtml" "siscweb/html"
   "ssax/ssax" "ssax/util"
   "sxpath/sxpathlib" "sxpath/sxpath-ext"
   "sxpath/xpath-parser" "sxpath/txpath" "sxpath/sxml-tools"
   "sxpath/sxpath" "lshift/sxml-tools"
   "siscweb/xml"))
