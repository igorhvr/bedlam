; (define* (g a (b a) (key: k (* a b))) (list a b k)) ; to test

(require-extension (lib iasylum/srfi-88))

(module iasylum/srfi-89
  (define* lambda* $hash-keyword $perfect-hash-table-lookup $undefined $req-key $opt-key $process-keys)
  (include "srfi-89-code.scm"))
