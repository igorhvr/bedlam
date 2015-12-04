(define (lzw-compress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.compressToUTF16(v);" `((v ,(->jstring value))))))

(define (lzw-decompress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.decompressFromUTF16(v);" `((v ,(->jstring value))))))


;; Sample roundtrip working:
;;(js "iasylum.compression.lzw_decompress(nv);" `((nv ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring (->string (js "iasylum.compression.lzw_decompress(v);" `((v ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring "aline")))))))))))))))))
