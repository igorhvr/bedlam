;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm")
(define sjcl.js (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/sjcl/sjcl-min.js"))))
(define sjcl.js-unsafe (memoize (lambda () (file->string "/base/bedlam/external-tools/libraries/sjcl/sjcl-unsafenonrandom-1.0.0.min.js"))))
(define iasylum.js (memoize (lambda () (file->string "/base/bedlam/iasylum/iasylum.js"))))
(define crypto.js (memoize (lambda () (file->string "/base/bedlam/iasylum/crypto/crypto.js"))))

(define load-safe-sjcl-and-add-entropy
  (lambda* ((js-manager: js-manager (get-local-javascript-manager)))
           (js (sjcl.js) #f js-manager)
           (js "sjcl.random.addEntropy(prn, 1024, 'nativeprgn-secure-random');"
               `((prn ,(j "r=new byte[128]; java.security.SecureRandom.getInstance(\"NativePRNG\").nextBytes(r);Arrays.toString(r);" '((r)))))
               js-manager)
           ;; For side effects only.
           (void)
           ))

(define crypto/prepare-javascript-manager
  (lambda* ((js-manager: js-manager (get-local-javascript-manager)))
           (load-safe-sjcl-and-add-entropy 'js-manager: js-manager)
           (js (iasylum.js) #f js-manager)
           (js (crypto.js) #f js-manager)
           ;; For side-effects only.
           (void)
           ))

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
               (crypto/prepare-javascript-manager)
               (begin 
                 (js (sjcl.js-unsafe))(js (iasylum.js))(js (crypto.js))))

           (unless string-to-generate-deterministically-from
             (js "sjcl.random.addEntropy(prn, 1024, 'nativeprgn-secure-random');"
                 `((prn ,(j "r=new byte[128]; java.security.SecureRandom.getInstance(\"NativePRNG\").nextBytes(r);Arrays.toString(r);" '((r)))))))

           (when string-to-generate-deterministically-from
             (and-let* (
                        (seed (get-seed-from string-to-generate-deterministically-from))
                        (random-function (random-maker seed))
                        (pseudo-random-number (random-function (expt 10 12))))

               (js "sjcl.random.addEntropy(prn, 1024, 'string-sourced-deterministic-pseudo-random-number');"
                   `((prn ,(string-append* pseudo-random-number string-to-generate-deterministically-from))))))
           ;; TODO - allow non-unsafe generation of keys from strings using something similar to:
           ;;(js "sjcl.ecc.elGamal.generateKeys(sjcl.ecc.curves['c256'], 10, 0xa0a0bc893f1681c0eb5fad86bac1d784ccdb2cebe68a13362b4c0c8495ee9cd0 ).sec.get;")

           (match
            (json->scheme (->string (if (not string-to-generate-deterministically-from)
                                        (js "iasylum.crypto.generate_sjcl_el_gammal_ecc_c256_keypair();")
                                        (js "iasylum.crypto.generate_unsafe_sjcl_el_gammal_ecc_c256_keypair();"))))
            (#( ( "publicKey" . the-public-key )
                ( "secretKey" . the-secret-key ) ) `(,(scheme->json the-public-key) ,(scheme->json the-secret-key))))))

(define (asymmetric-encrypt key data)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.asymmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (asymmetric-decrypt key data)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.asymmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-encrypt key data)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.symmetric_encrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (symmetric-decrypt key data)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.symmetric_decrypt(key, data);" `((key ,(->jstring key)) (data ,(->jstring data))))))

(define (hmac key data)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.hmac(key, data);" `((key ,(->jstring key))
                                                    (data ,(->jstring data))))))

;;
;; - simple-date is today in format YYYYMMDD
;; - service is "s3" e.g.
;;
(define (aws-signature what secret simple-date region service)
  (crypto/prepare-javascript-manager)  
  (->string (js "iasylum.crypto.aws_signature(a, b, c, d, e);"
                `((a ,(->jstring what))
                  (b ,(->jstring secret))
                  (c ,(->jstring simple-date))
                  (d ,(->jstring region))
                  (e ,(->jstring service))))))

;; Sample usage:
;; (load "/base/bedlam/iasylum/crypto/crypto-code.scm") (define keypair (generate-keypair 'sjcl_el_gammal_ecc_c256_key))(define keypair-pub (match keypair ((public secret) public)))(define keypair-sec (match keypair ((public secret) secret))) (asymmetric-decrypt keypair-sec (asymmetric-encrypt keypair-pub "1234567"))

(define* (openssl-rsautl/encrypt (pkcs8-key-string: pkcs8-key-string) (data: data))
    (let* ((pkcs8-key-file (string->file pkcs8-key-string))
          (data-file (string->file data))
          (command (string-append "openssl rsautl -encrypt -oaep -pubin -inkey "
                      pkcs8-key-file " -in " data-file  " -out - | base64")))
      (r/s command)))

(define* (openssl-rsautl/decrypt (rsa-key-string: rsa-key-string) (data: data))
    (let* ((rsa-key-file (string->file rsa-key-string))
           (data-file (string->file data))
           (command (string-append "base64 -d " data-file  " | openssl rsautl -decrypt -oaep -inkey " rsa-key-file  " -in - -out -")))
      (r/s command)))

;; We will use this to get the format needed for encryption from a provided public key.
(define (openssl-rsautl/chef-pub-or-sec-key-to-pub-pkcs8-format key)
  (let ((key-file (string->file key)))
    (r/s "chmod 600 " key-file)
    (let ((result (r/s "ssh-keygen -e -f " key-file " -m PKCS8")))
      (secure-file-delete! key-file)
      result)))

(define (ssh-keygen/extract-fingerprint-from-ssh-key key)
  (let ((key-file (string->file key)))
    (r/s "chmod 600 " key-file)
    (let ((result (r/s "ssh-keygen -l -f " key-file)))
      (secure-file-delete! key-file)
      result)))

(define (openssl-rsautl/test-encryption-roundtrip)
  (assert (string=? "round-trip-data"
                    (openssl-rsautl/decrypt 'rsa-key-string: "-----BEGIN RSA PRIVATE KEY-----\nMIIEogIBAAKCAQEAu5u3Y/mlEu6HvheSJTnvOx1vrMGQJBNK8N+B5QhaLORDZ56r\n6pD245xwpeAnvjbk9WyWpsufhZJY16vTN+kQKy411h5aqyLT8MpZc4pjEBgeivHJ\n0HlnKoa+3k6f/L+lzBJUYR8JuBi+g69Q6/WDSAdfSeIVbwo8ZQ8HYr7mHDBoHw9C\n0VzIuXVaaKvgdiUloMGlDECyJSJCsFLtVP4UOyCP1khP0IUR6MwB3bNEPwCUqGiF\nf7Nw11a+AEbD/JBbMZ3xynoxy9U2tZZjjkJhF4qtj7zLCqovfNvBtvzYftod3Aoc\nM85bcvY3RmFCrSAf6pIfHNgQ5B+VlWqprfmMzQIDAQABAoIBACJQefukRsgurs9b\nGlUKwrIKUUnE4atnh/aEuwp8O5ooahfC0ukFeNLq40PDuyE0gy5MnUWGyvewa+WO\nvQRl8ZokSp6OUMEqjp9lM3VJo5LnBncdgG9MNU129eRNdz/Qge/QjnRxK+LrS7Vk\nVKXD8y4ygwBNhOQZeDB3zj8GcapH9CgZirt5kVcbM24wCXD7+zrU865cf1goiIaW\njtaew1OB7yfwbrswy0u9b/VVupm+Ek1jrgPfhFbwztvbLwwceO77patBqEkIJPFp\nb0ycghq009f97XCDOl+mBidHre3W/AwQi5TyfAaqT0JNn0CKZy49E9CQukgDb+QG\nmePDOCECgYEA28KykfaTH7esQQQo9qosca+A4+m9yrFA+P3qfDX4y/VcqEyi8JLj\n32Q2vsRjFzangx82eGuG1yoXzg6vRyH8+yUKsGMi+J7OQoN6PHFgX+76NxYuS+0W\nNpeYQE4fg5G0RVCQ9vSYaNApyzoK9phdRys0V/8JYHdqE1vTztkJnTUCgYEA2oux\nx1I/2dA/ksu9EynuUa9trW/9Iis/0YAGuhepQiimnE0x0bgu6JIFn/un3fCFZ1Ck\n2tYqPx2T5ckDd9QoW2I5PtJUZIlBr0mzZ8zzHfwv4IjwK/ycWK6VJ9AQBwyxdJ3p\nncA+v0qyJ7yzjI2EmTg9CYV1yRNWlMuSVDxM3DkCgYBLAUixSNcuHCJOjnzss2g3\n5Q64uy5r39OtJ/zAKCuicTwOtRlnwrrDpBCLS7wGUEEcH6sXrpt3FIbLbXelb5RI\no3vid/OXp5v+V6GAv7GFDKuZ4Zgrkd/jAhqU2BUpcrF0dusDXrgmDeY11rmnMJml\nkLlszz8EDb3GnbDNCIafgQKBgBeiCZyDUXJNacKHE7Ax1ZqxvMuHk7kRMjqGfLO9\nAUmtOa8nTd7e6vutZrxRK2r9qn9sohckF7dxjF/J5/0aTS7spUIc3pFsolTBRIxS\nBmxUrBy80jP/giy43FpMzp7kiYAR34R2mJJ5EmDnsAbf+tnS0g7ohr8yvyciCHXh\ne3JJAoGAJGWFV4ReA8IlvrJBTJY2nzFoplEw5RqvtWhukXlCsHpjICwe9AQqo3mu\ncwMyQpY307WQx8bJ4tC8Wu5MQz4HVK+J5LgbieRG/k5eD6zaF1hGWk7cutr+fekD\nPZKFFe1BjBqapasV4nBNgrZqoqf3l317uHBjprkCHykRO2ZSHiQ=\n-----END RSA PRIVATE KEY-----\n"
                             'data: (openssl-rsautl/encrypt 'data: "round-trip-data"
                                             'pkcs8-key-string: "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu5u3Y/mlEu6HvheSJTnv\nOx1vrMGQJBNK8N+B5QhaLORDZ56r6pD245xwpeAnvjbk9WyWpsufhZJY16vTN+kQ\nKy411h5aqyLT8MpZc4pjEBgeivHJ0HlnKoa+3k6f/L+lzBJUYR8JuBi+g69Q6/WD\nSAdfSeIVbwo8ZQ8HYr7mHDBoHw9C0VzIuXVaaKvgdiUloMGlDECyJSJCsFLtVP4U\nOyCP1khP0IUR6MwB3bNEPwCUqGiFf7Nw11a+AEbD/JBbMZ3xynoxy9U2tZZjjkJh\nF4qtj7zLCqovfNvBtvzYftod3AocM85bcvY3RmFCrSAf6pIfHNgQ5B+VlWqprfmM\nzQIDAQAB\n-----END PUBLIC KEY-----\n")))))
