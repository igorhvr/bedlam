(module parser-error

  (x->string parser-error)

  (define (x->string thing)
    (format "~s" thing))

  (define (parser-error port message . rest)
    (error 'parser-error (string-append
                          message
                          (apply string-append
                                 (map (lambda (thing)
                                        (string-append " " (x->string thing)))
                                      rest)))))
  )
