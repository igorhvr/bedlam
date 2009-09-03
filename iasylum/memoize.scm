(require-extension (lib iasylum/srfi-89))
(require-extension (lib iasylum/match))

(module iasylum/memoize
  (memoize)

  (define (memoize func)
    (let ((memoresults '()))
      (lambda args
        (let ((memoized-pair (assoc args memoresults)))
                                        ; for debugging:
                                        ; (if memoized-pair (begin (print memoized-pair) (cdr memoized-pair))
          (if memoized-pair (cdr memoized-pair)
              (let ((thing-to-save (apply func args)))
                (begin
                  (set! memoresults (cons (cons args thing-to-save) memoresults))
                  thing-to-save)))))))
  )
