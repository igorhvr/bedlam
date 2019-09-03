(require-extension (lib iasylum/javascript))

;; This is a wrapper of the sjcl designed to allow encrypted exchanges between a bedlam-based service and a js-based peer.
(module iasylum/crypto
  (generate-keypair
   get-seed-from
   asymmetric-encrypt asymmetric-decrypt
   symmetric-encrypt symmetric-decrypt
   crypto/prepare-javascript-manager
   hmac
   aws-signature
   openssl-rsautl/encrypt openssl-rsautl/decrypt
   openssl-rsautl/chef-pub-or-sec-key-to-pub-pkcs8-format
   openssl-rsautl/test-encryption-roundtrip
   ssh-keygen/extract-fingerprint-from-ssh-key)

  (include "crypto/crypto-code.scm"))
