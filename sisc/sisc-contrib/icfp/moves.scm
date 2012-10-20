; Moves

(define (package-move? move)
  (memq (car move) '(|Pick| |Drop|)))
