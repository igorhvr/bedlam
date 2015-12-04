(define (lzw-compress value)
  (d/n "About to compress " value)
  (let ((result (->string (j "com.diogoduailibe.lzstring4j.LZString.compressToBase64(v);" `((v ,(->jstring value)))))))
    (d/n "Success. Result: " value)))

(define (lzw-decompress value)
  (d/n "About to decompress " value)
  (let ((result (->string (j "com.diogoduailibe.lzstring4j.LZString.decompressFromBase64(v);" `((v ,(->jstring value)))))))
    (d/n "Success. Result: " value)))


;; Sample roundtrip working:
;;(js "iasylum.compression.lzw_decompress(nv);" `((nv ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring (->string (js "iasylum.compression.lzw_decompress(v);" `((v ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring "aline")))))))))))))))))


;;(->string
;; (j "com.diogoduailibe.lzstring4j.LZString.decompressFromBase64(v);"
;;    `((v ,(js "LZString.compressToBase64(v);"
;;              `((v ,(->jstring (->string (js "LZString.decompressFromBase64(v);"
;;                                             `((v ,(->string
;;                                                    (j "com.diogoduailibe.lzstring4j.LZString.compressToBase64(v);"
;;                                                       `((v ,(->jstring "aline")))))))))))))))))
