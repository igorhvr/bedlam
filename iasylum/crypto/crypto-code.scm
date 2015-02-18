;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm")
(define sjcl.js-unsafe (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/sjcl/custom-built/sjcl-unsafenonrandom-1.0.0.min.js"))))
(define iasylum.js (memoize (lambda () (file->string "/base/bedlam/iasylum/iasylum.js"))))
(define crypto.js (memoize (lambda () (file->string "/base/bedlam/iasylum/crypto/crypto.js"))))

(define (get-seed-from str-p)
  (let* ( (str (sha256 str-p))
          (magic-str-tied-number           
           (logxor (fold logxor 0 (map char->integer (string->list str)))
                   (fold * 1
                         (map (lambda (v) (+ 1 v)) (map char->integer (string->list str)))))) )
          magic-str-tied-number))

;; Returns two strings, the first representing a public key and the second the corresponding private key.
(define generate-keypair
  (lambda* (type (string-to-generate-deterministically-from: string-to-generate-deterministically-from #f))
  ;; We only support this type of keypair for now.
           (assert (eqv? type 'sjcl_el_gammal_ecc_c192_key))
  
           (js (sjcl.js-unsafe)) (js (iasylum.js)) (js (crypto.js))

           (unless string-to-generate-deterministically-from
             (js "sjcl.random.addEntropy(prn, 1024, 'scheme-random-number');"
                 ;; FIXXXME TODO Use a secure random source, adjust number of bits above.
                 `((prn ,(number->string (random))))))

           (when string-to-generate-deterministically-from
             (and-let* (
                        ((js (sjcl.js-unsafe)))
                        (seed (get-seed-from string-to-generate-deterministically-from))
                        (random-function (random-maker seed))
                        (pseudo-random-number (random-function (expt 10 12))))

               (js "sjcl.random.addEntropy(prn, 1024, 'string-sourced-deterministic-pseudo-random-number');"
                   `((prn ,pseudo-random-number)))))
           
           (match
            (json->scheme (->string (js "iasylum.crypto.generate_sjcl_el_gammal_ecc_c192_keypair();")))
            (#( ( "publicKey" . the-public-key )
                ( "secretKey" . the-secret-key ) ) `(,(scheme->json the-public-key) ,(scheme->json the-secret-key))))))

(define (asymmetric-encrypt key data)
  (js (sjcl.js-unsafe)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.asymmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (asymmetric-decrypt key data)
  (js (sjcl.js-unsafe)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.asymmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-encrypt key data)
  (js (sjcl.js-unsafe)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.symmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-decrypt key data)
  (js (sjcl.js-unsafe)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.symmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))


;; Sample usage:
;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm") (define keypair (generate-keypair 'sjcl_el_gammal_ecc_c192_key))(define keypair-pub (match keypair ((public secret) public)))(define keypair-sec (match keypair ((public secret) secret))) (asymmetric-decrypt keypair-sec (asymmetric-encrypt keypair-pub "1234567"))