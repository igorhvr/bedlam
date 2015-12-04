(define (lzw-compress value)
  (d/n "COMPL: lzw-compress for " value)
  (let ((result (->string (j "com.diogoduailibe.lzstring4j.LZString.compressToUTF16(v);" `((v ,(->jstring value)))))))
    (d/n "COMPL: Success in compression. Result: " (string-length result) "/" (string-length value))
    (d/n "COMPL: lzw-compress returning " result " for " value)
    result))

(define (lzw-decompress value)
  (d/n "COMPL: lzw-decompress for " value)
  (let ((result (->string (j "com.diogoduailibe.lzstring4j.LZString.decompressFromUTF16(v);" `((v ,(->jstring value)))))))
    (d/n "COMPL: Success in decompression. Result: " (string-length value) "/" (string-length result))
    (d/n "COMPL: lzw-decompress returning " result " for " value)
    result))


;; Sample roundtrip working:
;;(js "iasylum.compression.lzw_decompress(nv);" `((nv ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring (->string (js "iasylum.compression.lzw_decompress(v);" `((v ,(->string (j "com.diogoduailibe.lzstring4j.LZString.compress(v);" `((v ,(->jstring "aline")))))))))))))))))


;;(->string
;; (j "com.diogoduailibe.lzstring4j.LZString.decompressFromBase64(v);"
;;    `((v ,(js "LZString.compressToBase64(v);"
;;              `((v ,(->jstring (->string (js "LZString.decompressFromBase64(v);"
;;                                             `((v ,(->string
;;                                                    (j "com.diogoduailibe.lzstring4j.LZString.compressToBase64(v);"
;;                                                       `((v ,(->jstring "aline")))))))))))))))))
