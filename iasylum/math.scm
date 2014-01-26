(module iasylum/math
  (fact)
  
  (define (fact n) (if (or (= 0 n) (= 1 n)) 1 (* n (fact (- n 1))))))