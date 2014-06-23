(require-extension (lib iasylum/jcode))

(module iasylum/scrypt
  (scrypt-hash scrypt-check)

  ; See scrypt.pdf for details about the parameters.
  (define scrypt-hash
    (match-lambda*
     ((passwd)
      (scrypt-hash passwd 16384 8 1))
     ((passwd N r p)
      (->string (j "com.lambdaworks.crypto.SCryptUtil.scrypt(passwd, n, r, p);" `((passwd ,(->jobject passwd)) (n ,(->jobject N)) (r ,(->jobject r)) (p ,(->jobject p))))))
     ((passwd salt N r p)         
      (->string (j "
		 int log2(int n) {
		     int log = 0;
		     if ((n & 0xffff0000 ) != 0) { n >>>= 16; log = 16; }
		     if (n >= 256) { n >>>= 8; log += 8; }
		     if (n >= 16 ) { n >>>= 4; log += 4; }
		     if (n >= 4 ) { n >>>= 2; log += 2; }
		     return log + (n >>> 1);
		 }
		 salt = saltstring.getBytes(\"UTF-8\");
		 derived = com.lambdaworks.crypto.SCrypt.scrypt(passwd.getBytes(\"UTF-8\"), salt, n, r, p, 32);
		 params = Long.toString(log2(n) << 16L | r << 8 | p, 16);
		 sb = new StringBuilder((salt.length + derived.length) * 2);
		 sb.append(\"$s0$\").append(params).append('$');
		 sb.append(com.lambdaworks.codec.Base64.encode(salt)).append('$');
		 sb.append(com.lambdaworks.codec.Base64.encode(derived));
		 return sb.toString();" 
                   `((passwd ,(->jobject passwd)) (saltstring ,(->jobject salt)) (n ,(->jobject N)) (r ,(->jobject r)) (p ,(->jobject p))))))))

  (define (scrypt-check passwd hashed)
    (->boolean (j "com.lambdaworks.crypto.SCryptUtil.check(passwd, hashed);" `((passwd ,(->jobject passwd)) (hashed ,(->jobject hashed)))))))
           
