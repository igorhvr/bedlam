;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

;;; This allows one to easily call Clojure code from inside Scheme code.

(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/random))

(module iasylum/clojure
  (clojure/run
   clojure/repl-start
   clj

   ; aux:
   clojure/find-value-by-key
   symbol->clj-keyword
   clj-keyword->symbol
   symbol->clj-symbol
   clj-keyword->string
   clj-number->number
   number->clj-number
   list->persistent-vector
   alist->persistent-map
   persistent-vector-size
   map-persistent-vector
   only-persistent-vector
   get-keys-persistent-hash-map
   get-value-persistent-hash-map
   get-iterator-from-persistent-hash-set
   persistent-hash-set->jarray
   get-persistent-vector
   clojure-read
   get-native-clojure-fn-as-lambda
   call-native-clojure-fn
   load-clojure-namespace
   clojure-sort-persistent-vector
   )
  
  (include "clojure-code.scm"))
