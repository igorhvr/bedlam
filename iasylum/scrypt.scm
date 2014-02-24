(require-extension (lib iasylum/jcode))

(module iasylum/scrypt
  (scrypt-hash scrypt-check)

  ; See scrypt.pdf for details about the parameters.
  (define (scrypt-hash passwd N r p)
    (->string (j "com.lambdaworks.crypto.SCryptUtil.scrypt(passwd, n, r, p);" `((passwd ,(->jobject passwd)) (n ,(->jobject N)) (r ,(->jobject r)) (p ,(->jobject p))))))

  (define (scrypt-check passwd hashed)
    (->boolean (j "com.lambdaworks.crypto.SCryptUtil.check(passwd, hashed);" `((passwd ,(->jobject passwd)) (hashed ,(->jobject hashed)))))))
           