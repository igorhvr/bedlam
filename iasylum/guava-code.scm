(define make-cache
  (lambda* ((size #f) (spec: spec #f))
      (when (and (not size) (not spec))
        (error "missing parameters. Examples:
              ; See https://google.github.io/guava/releases/16.0/api/docs/com/google/common/cache/CacheBuilderSpec.html
              (make-cache #f 'spec: \"expireAfterWrite=120s,maximumSize=16384\")

              ; Simple version with maximum size only.
              (make-cache 16) ; 16 slots maximum size"))
      (assert (xor spec size))
      (if size
          (j "com.google.common.cache.CacheBuilder.newBuilder()
                     .maximumSize(size)
                     .build();"
             `((size ,(->jint size))))
          (j "com.google.common.cache.CacheBuilder.from(spec).build();"
             `((spec ,(->jstring spec)))))))

;; Spec is explained at
;; https://google.github.io/guava/releases/16.0/api/docs/com/google/common/cache/CacheBuilderSpec.html
; in the following form:
; A specification of a CacheBuilder configuration.  

; CacheBuilderSpec supports parsing configuration off of a string, which
; makes it especially useful for command-line configuration of a
; CacheBuilder.
;    
; The string syntax is a series of comma-separated keys or key-value
; pairs, each corresponding to a CacheBuilder method. 
; 
;     concurrencyLevel=[integer]: sets CacheBuilder.concurrencyLevel.
;     initialCapacity=[integer]: sets CacheBuilder.initialCapacity.
;     maximumSize=[long]: sets CacheBuilder.maximumSize.
;     maximumWeight=[long]: sets CacheBuilder.maximumWeight.
;     expireAfterAccess=[duration]: sets CacheBuilder.expireAfterAccess(long, java.util.concurrent.TimeUnit).
;     expireAfterWrite=[duration]: sets CacheBuilder.expireAfterWrite(long, java.util.concurrent.TimeUnit).
;     refreshAfterWrite=[duration]: sets CacheBuilder.refreshAfterWrite(long, java.util.concurrent.TimeUnit).
;     weakKeys: sets CacheBuilder.weakKeys().
;     softValues: sets CacheBuilder.softValues().
;     weakValues: sets CacheBuilder.weakValues().
;     recordStats: sets CacheBuilder.recordStats().    
; 
; The set of supported keys will grow as CacheBuilder evolves, but
; existing keys will never be removed.
; 
; Durations are represented by an integer, followed by one of "d", "h",   
; "m", or "s", representing days, hours, minutes, or seconds
; respectively. (There is currently no syntax to request expiration in
; milliseconds, microseconds, or nanoseconds.) 
; 
; Whitespace before and after commas and equal signs is ignored. Keys   
; may not be repeated; it is also illegal to use the following pairs of
;                    ; keys in a single value:   
; 
;     maximumSize and maximumWeight
;     softValues and weakValues 
; 
; CacheBuilderSpec does not support configuring CacheBuilder methods
; with non-value parameters. These must be configured in code. 
; 
; A new CacheBuilder can be instantiated from a CacheBuilderSpec using
; CacheBuilder.from(CacheBuilderSpec) or CacheBuilder.from(String). 

(define (cache-put! cache key value)
  (j "cache.put(key, value);" `((cache ,cache)
                                (key ,(->jstring key))
                                (value ,(java-wrap value))))
  value)

(define (cache-get cache key)
  (let ((result (j "cache.getIfPresent(key);" `((cache ,cache)
                                                (key ,(->jstring key))))))
    (and (not (java-null? result))
         (java-unwrap result))))
