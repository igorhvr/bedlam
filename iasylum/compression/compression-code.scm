(define (lzw-compress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.compressToUTF16(v);" `((v ,value)))))

(define (lzw-uncompress value)
  (->string (j "com.diogoduailibe.lzstring4j.LZString.decompressFromUTF16(v);" `((v ,value)))))
