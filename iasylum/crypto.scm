(require-extension (lib iasylum/javascript))

;; This is a wrapper of the sjcl designed to allow encrypted exchanges between a bedlam-based service and a js-based peer.
(module iasylum/crypto
  (generate-keypair
   asymmetric-encrypt asymmetric-decrypt
   symmetric-encrypt symmetric-decrypt)

  (include "crypto/crypto-code.scm"))