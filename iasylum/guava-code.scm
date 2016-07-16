(define (make-cache size)
  (j "com.google.common.cache.CacheBuilder.newBuilder()
                     .maximumSize(size)
                     .build();"
     `((size ,(->jint size)))))

(define (cache-put! cache key value)
  (j "cache.put(key, value);" `((cache ,cache)
                                (key ,(->jstring key))
                                (value ,(java-wrap value))))
  value)

(define (cache-get cache key)
  (let ((result (j "cache.getIfPresent(key);" `((cache ,cache)
                                                (key ,(->jstring key))))))
    (and (not (java-null? result))
         result)))
