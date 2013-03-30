
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for Chicken
;; (require-extension syntax-case)
;; (define random-integer random)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from http://okmij.org/ftp/Scheme/keyword-arg-macro.txt

(define-syntax let-keyword-form
  (syntax-rules ()
    ((let-keyword-form
      ((labeled-arg-macro-name
        (positional-form-name (arg-name . arg-default) ...)))
      . body)
     (letrec-syntax
         ((labeled-arg-macro-name
           (syntax-rules ()
             ((labeled-arg-macro-name . keyword-val-pairs)
              (letrec-syntax
                  ((find
                    (syntax-rules (<- arg-name ...)
                      ((find kvp k-args (arg-name . default) arg-name <- val
                             . others) ; found arg-name among keyword-val-pairs
                       (next kvp val . k-args)) ...
                      ((find kvp k-args key arg-no-match-name <- val . others)
                       (find kvp k-args key . others))
                      ((find kvp k-args (arg-name default)) ; default must be here
                       (next kvp default . k-args)) ...
                      ))
                   (next               ; pack the continuation to find
                    (syntax-rules ()
                      ((next kvp val vals key . keys)
                       (find kvp ((val . vals) . keys) key . kvp))
                      ((next kvp val vals) ; processed all arg-descriptors
                       (rev-apply (val) vals))))
                   (match-positionals
                    (syntax-rules (<-)
                      ((match-positionals () res . rest)
                       (rev-apply () res))
                      ((match-positionals args (val . vals) name <- value . rest)
                       (next (name <- value . rest) val vals . args))
                      ((match-positionals args (val . vals))
                       (next () val vals . args))
                      ((match-positionals (arg1 . args) res pos-arg . rest)
                       (match-positionals args (pos-arg . res) . rest))))
                   (rev-apply
                    (syntax-rules ()
                      ((rev-apply form (x . xs))
                       (rev-apply (x . form) xs))
                      ((rev-apply form ()) form))))
                (match-positionals ((arg-name . arg-default) ...)
                                   (positional-form-name)
                                   . keyword-val-pairs)
                )))))
       . body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax loop
  (syntax-rules ()
    ;; unnamed, implicit recursion
    ((loop (vars ...) body ...)
     (%loop tmp-loop () () () () () (vars ...) body ... (tmp-loop)))
    ;; named, explicit recursion
    ((loop name (vars ...) body ...)
     (%loop name () () () () () (vars ...) body ...))))

(define-syntax %loop
  (syntax-rules (=> <-)
    ;; automatic iteration
    ((_ name l v c r f ((var1 <- iterator source ...) rest ...) . body)
     (iterator ((var1) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 var3 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2 var3) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 var3 var4 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2 var3 var4) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ;; do equivalents, with optional guards
    ((_ name l (vars ...) (checks ...) r f ((var init step guard) rest ...) . body)
     (%loop name l (vars ... (var init step)) (checks ... (guard var)) r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((var init step) rest ...) . body)
     (%loop name l (vars ... (var init step)) c r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((var init) rest ...) . body)
     (%loop name l (vars ... (var init var)) c r f (rest ...) . body))
    ;; specify a default done?
    ((_ name l v c r f ())
     (%loop name l v c r f () (#f #f)))
    ((_ name l v c r f () () . body)
     (%loop name l v c r f () (#f #f) . body))
    ;; final expansion
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        => result
        . body)
     (let* (lets ...)
       (letrec ((tmp (lambda (var ...)
                       (if (or checks ...)
                         (let-keyword-form ((name (tmp (var step) ...)))
                            (let (finals ...)
                              result))
                         (let (refs ...)
                           (let-keyword-form ((name (tmp (var step) ...)))
                             (if #f #f)
                             . body))))))
         (tmp init ...))))
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        . body)
     (%loop name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
            => (if #f #f) . body))
    ))

(define-syntax %loop-next
  (syntax-rules ()
    ((_ (new-lets ...) (new-vars ...) (new-checks ...) (new-refs ...) (new-finals ...)
        name (lets ...) (vars ...) (checks ...) (refs ...) (finals ...)
        . rest)
     (%loop name (lets ... new-lets ...) (vars ... new-vars ...)
                 (checks ... new-checks ...) (refs ... new-refs ...)
                 (finals ... new-finals ...)
        . rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterators

;; Each gets passed two lists, those items left of the <- and those to
;; the right, followed by a NEXT and REST continuation.

;; Should finish with
;;
;;  (next (outer-vars) (cursor-vars) (done?-tests) (loop-vars) . rest)
;;
;;  OUTER-VARS: bound once outside the loop in a LET*
;;  CURSOR-VARS: DO-style bindings of the form (name init update)
;;  DONE?-TESTS: possibly empty list of forms that terminate the loop on #t
;;  LOOP-VARS: inner variables, updated in parallel after the cursors

(define-syntax in-list                  ; called just "IN" in ITER
  (syntax-rules ()
    ((in-list ((var) source) next . rest)
     (in-list ((var cursor) source) next . rest))
    ((in-list ((var cursor) (source)) next . rest)
     (in-list ((var cursor) (source cdr)) next . rest))
    ((in-list ((var cursor) (source step)) next . rest)
     (next ()                              ; outer let bindings
           ((cursor source (step cursor))) ; iterator, init, step
           ((null? cursor))                ; finish tests for iterator vars
           ((var (car cursor)))            ; step variables and values
           ()                              ; final result bindings
           . rest))))

(define-syntax in-string
  (syntax-rules ()
    ((in-string ((var) (source)) next . rest)
     (in-string ((var cursor) (source)) next . rest))
    ((in-string ((var cursor) (source)) next . rest)
     (next ((tmp-str source) (len (string-length tmp-str)))
           ((cursor 0 (+ cursor 1)))
           ((>= cursor len))
           ((var (string-ref tmp-str cursor)))
           ()
       . rest))))

(define-syntax in-vector
  (syntax-rules ()
    ((in-vector (ls ...) next . rest)
     (%in-vector >= + 0 (vector-length tmp-vec)
                 tmp-vec (ls ...) next . rest))))

(define-syntax in-vector-reverse
  (syntax-rules ()
    ((in-vector (ls ...) next . rest)
     (%in-vector < - (- (vector-length tmp-vec) 1) 0
                 tmp-vec (ls ...) next . rest))))

(define-syntax %in-vector
  (syntax-rules ()
    ((%in-vector ge + s e tmp-vec ((var) (vec ...)) next . rest)
     (%in-vector ge + s e tmp-vec ((var vec-index) (vec ...)) next . rest))
    ((%in-vector ge + s e tmp-vec ((var index) (vec)) next . rest)
     (%in-vector ge + s e tmp-vec ((var index) (vec s e 1)) next . rest))
    ((%in-vector ge + s e tmp-vec ((var index) (vec from)) next . rest)
     (%in-vector ge + s e tmp-vec ((var index) (vec from e 1)) next . rest))
    ((%in-vector ge + s e tmp-vec ((var index) (vec from to)) next . rest)
     (%in-vector ge + s e tmp-vec ((var index) (vec from to 1)) next . rest))
    ((%in-vector ge + s e tmp-vec ((var index) (vec from to step)) next . rest)
     (next ((tmp-vec vec) (end to))
           ((index from (+ index step)))
           ((ge index end))
           ((var (vector-ref tmp-vec index)))
           ()
       . rest))
    ))

(define-syntax in-port
  (syntax-rules ()
    ((in-port ((var) ()) next . rest)
     (in-port ((var) ((current-input-port) read-char)) next . rest))
    ((in-port ((var) (port)) next . rest)
     (in-port ((var) (port read-char)) next . rest))
    ((in-port ((var) (port reader)) next . rest)
     (next ((p port) (r reader))
           ((var (r p) (r p)))
           ((eof-object? var))
           ()
           ()
       . rest))))

(define-syntax in-file
  (syntax-rules ()
    ((in-file ((var) (file)) next . rest)
     (in-file ((var) (file read-char)) next . rest))
    ((in-file ((var) (file reader)) next . rest)
     (next ((p (open-input-file file)) (r reader))
           ((var (r p) (r p)))
           ((eof-object? var))
           ()
           ((dummy (close-input-port p)))
       . rest))))

(define-syntax in-range
  (syntax-rules ()
    ((in-range ((var) ()) next . rest)
     (next () ((var 0 (+ var 1))) () () . rest))
    ((in-range ((var) (from)) next . rest)
     (next () ((var from (+ var 1))) () () . rest))
    ((in-range ((var) (from to)) next . rest)
     (in-range ((var) (from to 1)) next . rest))
    ((in-range ((var) (from to step)) next . rest)
     (next ((tmp-to to))
           ((var from (+ var step)))
           ((>= var tmp-to))
           ()
           ()
       . rest))))

(define-syntax collecting
  (syntax-rules ()
    ((collecting ((var) source) next . rest)
     (collecting ((var cursor) source) next . rest))
    ((collecting ((var cursor) (source)) next . rest)
     (next ()
           ((cursor '() (cons source cursor)))
           ()
           ()
           ((var (reverse cursor)))
       . rest))))

(define random-real
  (let ((MAX_RAND (+ (expt 2 29) (- (expt 2 29) 1))))
    (lambda () (/ (random-integer MAX_RAND) MAX_RAND))))

(define-syntax in-random
  (syntax-rules ()
    ((in-random ((var) ()) next . rest)
     (next () ((var (random-real) (random-real))) () () . rest))
    ((in-random ((var) (n)) next . rest)
     (next ((tmp-n n))
           ((var (random-integer tmp-n) (random-integer tmp-n)))
           ()
           ()
           ()
        . rest))
    ((in-random ((var) (n lo)) next . rest)
     (next ((tmp-n n) (tmp-lo lo))
           ((var (+ tmp-lo (random-integer tmp-n))
                 (+ tmp-lo (random-integer tmp-n))))
           ()
           ()
           ()
       . rest))))

;; Johnson-Trotter
;;   Fast and uses constant space, but this implementation mutates the
;; cursor, which breaks re-entrant recursion.  Not tuned.

(define (next-permutation! state lefts len . o)
  (letrec
      ((mobile?
        (lambda (i)
          (let ((x (vector-ref state i)))
            (if (vector-ref lefts i)
              (and (positive? i) (< (vector-ref state (- i 1)) x))
              (and (< i (- len 1)) (< (vector-ref state (+ i 1)) x))))))
       (move!
        (lambda (i x)
          (if (vector-ref lefts i)
            (let ((j (- i 1)))
              (vector-set! state i (vector-ref state j))
              (vector-set! state j x)
              (vector-set! lefts i (vector-ref lefts j))
              (vector-set! lefts j #t))
            (let ((j (+ i 1)))
              (vector-set! state i (vector-ref state j))
              (vector-set! state j x)
              (vector-set! lefts i (vector-ref lefts j))
              (vector-set! lefts j #f)))
          (let lp ((j (- len 1)))
            (if (not (negative? j))
              (begin
                (if (< x (vector-ref state j))
                  (vector-set! lefts j (not (vector-ref lefts j))))
                (lp (- j 1)))))))
       (first-mobile
        (lambda ()
          (let lp ((i (- len 1)))
            (cond
              ((negative? i) #f)
              ((mobile? i) (next-mobile i))
              (else (lp (- i 1)))))))
       (next-mobile
        (lambda (i)
          (let lp ((i i) (x (vector-ref state i)) (j (- i 1)))
            (cond
              ((negative? j)
               (move! i x)
               (if (pair? o)
                 (let ((set (list->vector (car o))))
                   (let lp ((i (- len 1)) (res '()))
                     (if (negative? i)
                       res
                       (lp (- i 1)
                           (cons (vector-ref set (vector-ref state i)) res)))))
                 (vector->list state)))
              ((and (mobile? j) (< x (vector-ref state j)))
               (lp j (vector-ref state j) (- j 1)))
              (else
               (lp i x (- j 1))))))))
    (first-mobile)))

(define (make-vector-range n)
  (let ((res (make-vector n)))
    (let lp ((i (- n 1)))
      (if (negative? i)
        res
        (begin (vector-set! res i i) (lp (- i 1)))))))

(define-syntax in-permutations
  (syntax-rules ()
    ((in-permutations ((var) (set)) next . rest)
     (next ((tmp-set set)
            (len (length tmp-set))
            (vec (make-vector-range len))
            (lefts (make-vector len #t)))
           ((var set (next-permutation! vec lefts len tmp-set)))
           ((not var))
           ()
           ()
       . rest))))

(define (next-pair-bucket vec start end)
  (let lp ((i start))
    (and (< i end)
         (let ((x (vector-ref vec i)))
           (if (pair? x)
             i
             (lp (+ i 1)))))))

;; can't be implemented portably & efficiently (could use call/cc +
;; hash-table-for-each though)

; (define-syntax in-hash-table
;   (syntax-rules ()
;     ((in-hash-table ((key val) (table)) next . rest)
;      (next ((tmp-vec (##sys#slot table 1))
;             (end (vector-length tmp-vec))
;             (first-bucket (next-pair-bucket tmp-vec 0 end)))
;            ((bucket first-bucket
;                     (if (and (pair? cell) (pair? (cdr cell)))
;                       bucket
;                       (next-pair-bucket tmp-vec (+ bucket 1) end)))
;             (cell (and first-bucket (vector-ref tmp-vec first-bucket))
;                   (if (and (pair? cell) (pair? (cdr cell)))
;                     (cdr cell)
;                     (let ((i (next-pair-bucket tmp-vec (+ bucket 1) end)))
;                       (and i (vector-ref tmp-vec i))))))
;            ((not bucket))
;            ((key (caar cell))
;             (val (cdar cell)))
;        . rest))
;     ))

