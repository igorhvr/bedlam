(require-extension (lib iasylum/sparse/sparse-vectors/sparse-vectors)) 

(module iasylum/sparse/2d-array/2d-array

	(make-sparse-array
         sparse-array?
         sparse-array-ref
         sparse-array-set!)
       

;; this is a simple two dimensional sparse array

;; ONLY TWO DIMENSIONS!!! SEE ARRAY-LIB IF YOUR NEEDS ARE GREATER!!
;;
(define (make-sparse-array)
  (let ((a (make-sparse-vector)))
    (sparse-vector-set! a 0 (make-sparse-vector))
    a))

(define (sparse-array? a)
  (and (sparse-vector? a)
       (sparse-vector? (sparse-vector-ref a 0))))

(define (sparse-array-ref a x y)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-ref row y)
	#f)))

(define (sparse-array-set! a x y val)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-set! row y val)
	(let ((new-row (make-sparse-vector)))
	  (sparse-vector-set! a x new-row)
	  (sparse-vector-set! new-row y val)))))
)