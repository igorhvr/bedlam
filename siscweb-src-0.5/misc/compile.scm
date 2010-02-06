(require-library 'sisc/libs/srfi)

(import file-manipulation)

(define dest "../bin/")

(current-directory "../scm")

(for-each
 (lambda (name)
   (make-directory! (string-append dest name)))
 '("io" "lshift" "siscweb" "sxml" "util"))

(for-each
 (lambda (name)
   (display (string-append "Compiling " name " ..."))
   (compile-file (string-append name ".scm")
                 (string-append dest name ".scc"))
   (display "done.\n"))
 '("util/image" "util/jndi" "util/regexp"
   "util/misc" "util/srfi-71" "util/uid"
   "lshift/error" "lshift/common" "lshift/s2j-modutils"
   "siscweb/config"
   "siscweb/bindings" "siscweb/k-hash"
   "siscweb/context"
   "siscweb/frame" "siscweb/webcells"
   "siscweb/k-store"
   "siscweb/request" "siscweb/response" "siscweb/session"
   "siscweb/contcentric"
   "siscweb/publish"
   "siscweb/error" "siscweb/forward" "siscweb/redirect" "siscweb/text"
   "siscweb/image" "siscweb/sisclet"))
