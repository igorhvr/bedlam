(require-extension (lib iasylum/javascript))

(module iasylum/crypto
  (generate-keypair
   asymmetric-encrypt asymmetric-decrypt
   symmetric-encrypt symmetric-decrypt)

  (include "crypto/crypto-code.scm"))