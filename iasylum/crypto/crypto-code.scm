;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm")
(define sjcl.js (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/sjcl/sjcl-min.js"))))
(define sjcl.js-unsafe (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/sjcl/sjcl-unsafenonrandom-1.0.0.min.js"))))
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
           (assert (eqv? type 'sjcl_el_gammal_ecc_c256_key))

           (if (not string-to-generate-deterministically-from)
               (js (sjcl.js-unsafe))
               (js (sjcl.js-unsafe)))

           (js (iasylum.js)) (js (crypto.js))

           (unless string-to-generate-deterministically-from
             (js "sjcl.random.addEntropy(prn, 1024, 'nativeprgn-secure-random');"
                 `((prn ,(j "r=new byte[128]; java.security.SecureRandom.getInstance(\"NativePRNG\").nextBytes(r);Arrays.toString(r);")))))
           
           (when string-to-generate-deterministically-from
             (and-let* (
                        (seed (get-seed-from string-to-generate-deterministically-from))
                        (random-function (random-maker seed))
                        (pseudo-random-number (random-function (expt 10 12))))

               (js "sjcl.random.addEntropy(prn, 1048576, 'string-sourced-deterministic-pseudo-random-number');"
                   `((prn ,(string-append* pseudo-random-number string-to-generate-deterministically-from))))))
           ;; TODO - allow non-unsafe generation of keys from strings using something similar to:
           ;;(js "sjcl.ecc.elGamal.generateKeys(sjcl.ecc.curves['c256'], 10, 0xa0a0bc893f1681c0eb5fad86bac1d784ccdb2cebe68a13362b4c0c8495ee9cd0 ).sec.get;")
           (d/n "NUKBERARGH!\nARGH!\nARGH!\nARGH!\nARGH!\nARGH!\nARGH!\nARGH!\nARGH!=" string-to-generate-deterministically-from "\n\n")
           
           (match
            (json->scheme (->string (if (not string-to-generate-deterministically-from)
                                        (begin
                                          (d/n "SECURE")
                                          (js "iasylum.crypto.generate_sjcl_el_gammal_ecc_c256_keypair();"))
                                        (begin
                                          (d/n "INSECURE")
                                          (js "iasylum.crypto.generate_unsafe_sjcl_el_gammal_ecc_c256_keypair();")
                                          ))))
            (#( ( "publicKey" . the-public-key )
                ( "secretKey" . the-secret-key ) ) `(,(scheme->json the-public-key) ,(scheme->json the-secret-key))))))

(define (asymmetric-encrypt key data)
  (js (sjcl.js)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.asymmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (asymmetric-decrypt key data)
  (js (sjcl.js)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.asymmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-encrypt key data)
  (js (sjcl.js)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.symmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-decrypt key data)
  (js (sjcl.js)) (js (iasylum.js)) (js (crypto.js))
  (->string (js "iasylum.crypto.symmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (hmac key data)
  (js (sjcl.js))
  (js (iasylum.js))
  (js (crypto.js))
  (->string (js "iasylum.crypto.hmac(key, data);" `((key ,(->jstring key))
                                                    (data ,(->jstring data))))))


;; Sample usage:
;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm") (define keypair (generate-keypair 'sjcl_el_gammal_ecc_c256_key))(define keypair-pub (match keypair ((public secret) public)))(define keypair-sec (match keypair ((public secret) secret))) (asymmetric-decrypt keypair-sec (asymmetric-encrypt keypair-pub "1234567"))
