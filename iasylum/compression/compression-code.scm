(define (lzw-compress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.compressToUTF16(v);" `((v ,(->jstring value))))))

(define (lzw-uncompress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.decompressFromUTF16(v);" `((v ,(->jstring value))))))


;; Sample roundtrip working:
;;(js "iasylum.compression.lzw_uncompress(nv);" `((nv ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring (->string (js "iasylum.compression.lzw_uncompress(v);" `((v ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring "aline")))))))))))))))))
