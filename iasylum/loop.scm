(require-extension (lib iasylum/match))

(module iasylum/loop
  (random-integer let-keyword-form loop %loop %loop-next in-list in-string in-vector in-vector-reverse %in-vector in-port in-file in-range collecting random-real in-random next-permutation! make-vector-range in-permutations next-pair-bucket)

  (define random-integer random)

  (include "loop/loop.scm"))

;; Test/sample:
;;
;; (define (flatten-x ls)
;;     (reverse
;;      (loop lp ((x ls <- in-list ls) (res '()))
;;          => res
;;        (if (pair? x)
;;            (lp res <- (lp ls <- x))
;;            (lp res <- (cons x res))))))
;; (flatten-x '(1 (1 2 48 1 3) (1 (2 3 717174 81 (6 1))))) ===> (1 1 2 48 1 3 1 2 3 717174 81 6 1)