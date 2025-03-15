(module iasylum/ranked-pairs
  (rank-votes
   test-rank-votes)

;; From https://small.r7rs.org/wiki/SampleVoteYakHandler/ [ https://web.archive.org/web/20200514014222/https://small.r7rs.org/wiki/SampleVoteYakHandler/ ]
;; Fixed: remove should filter out elements that match the predicate
(define (remove pred ls)
  (let lp ((ls ls) (res '()))
    (cond ((null? ls) (reverse res))
          ((pred (car ls)) (lp (cdr ls) res))
          (else (lp (cdr ls) (cons (car ls) res))))))

;; Optimized union/eq using an accumulator for better performance
(define (union/eq a b)
  (let lp ((a a) (result b))
    (cond ((null? a) result)
          ((memq (car a) result) (lp (cdr a) result))
          (else (lp (cdr a) (cons (car a) result))))))

;; Optimized to process candidates in a single pass
(define (join-candidates ranked res)
  (let lp ((ls ranked) (res res))
    (cond ((null? ls) res)
          ((symbol? (car ls)) (lp (cdr ls) (if (memq (car ls) res) res (cons (car ls) res))))
          (else 
           (let inner-lp ((items (car ls)) (res res))
             (if (null? items)
                 (lp (cdr ls) res)
                 (inner-lp (cdr items) 
                           (if (memq (car items) res) res (cons (car items) res)))))))))

(define (extract-candidates votes)
  (let lp ((ls votes) (res '()))
    (if (null? ls)
        res
        (lp (cdr ls) (join-candidates (cdar ls) res)))))

;; Optimized to avoid unnecessary recursion
(define (vote-preferred? a b ls)
  (let lp ((ls ls))
    (and (pair? ls)
         (let ((tmp (if (symbol? (car ls)) (list (car ls)) (car ls))))
           (cond ((memq a tmp) (not (memq b tmp)))
                 ((memq b tmp) #f)
                 (else (lp (cdr ls))))))))

(define (tally-vote a b votes)
  (let lp ((ls votes) (res 0))
    (if (null? ls)
        res
        (lp (cdr ls) (if (vote-preferred? a b (cdar ls)) (+ res 1) res)))))

;; Optimized to compute all tallies in fewer passes
(define (tally-votes votes)
  (let ((candidates (extract-candidates votes)))
    (let lp1 ((ls1 candidates) (res '()))
      (if (null? ls1)
          res
          (let ((a (car ls1)))
            (let lp2 ((ls2 candidates) (res res))
              (cond
               ((null? ls2)
                (lp1 (cdr ls1) res))
               ((eq? a (car ls2))
                (lp2 (cdr ls2) res))
               (else
                (let ((b (car ls2)))
                  (lp2 (cdr ls2)
                       (cons (cons (cons a b)
                                   (tally-vote a b votes))
                             res)))))))))))

(define (pair-score pair pairs)
  (cond ((assoc pair pairs) => cdr) (else 0)))

(define inverted-sort (lambda (a b) (sort b a)))

(define (sort-pairs pairs)
  ;; requires SRFI-98 compatible `sort': (sort ls less?)
  (inverted-sort pairs
        (lambda (a b)
          (or (> (cdr a) (cdr b))
              (and (= (cdr a) (cdr b))
                   (let ((a^-1 (pair-score (cons (cdar a) (caar a)) pairs))
                         (b^-1 (pair-score (cons (cdar b) (caar b)) pairs)))
                     (< a^-1 b^-1)))))))

(define (insert-edge a b graph)
  (let lp ((ls graph) (rev '()))
    (cond
     ((null? ls)
      (cons (list a b) graph))
     ((equal? a (caar ls))
      (if (member b (cdar ls))
          graph
          (append (reverse rev)
                  (cons (cons (caar ls) (cons b (cdar ls))) (cdr ls)))))
     (else
      (lp (cdr ls) (cons (car ls) rev))))))

(define (graph-ref graph a)
  (cond ((assoc a graph) => cdr) (else '())))

;; Optimized graph-reachable? with better tracking of visited nodes
;; can a be reached from b with the given graph?
(define (graph-reachable? a b graph)
  (let lp ((queue (graph-ref graph b))
           (seen (list b)))
    (cond
     ((null? queue) #f)
     ((equal? a (car queue)) #t)
     (else
      (let ((next (car queue))
            (rest (cdr queue)))
        (if (member next seen)
            (lp rest seen)
            (lp (append rest 
                        (filter (lambda (x) (not (member x seen)))
                                (graph-ref graph next)))
                (cons next seen))))))))

(define (lock-pairs pairs)
  (let lp ((ls pairs) (graph '()))
    (cond
     ((null? ls)
      graph)
     ((graph-reachable? (caar ls) (cdar ls) graph)
      (lp (cdr ls) graph))
     (else
      (lp (cdr ls) (insert-edge (caar ls) (cdar ls) graph))))))

(define (topological-sort graph)
  (let visit ((ls graph) (seen '()) (res '()) (return (lambda (seen res) res)))
    (cond
     ((null? ls)
      (return seen res))
     ((member (car (car ls)) seen)
      (visit (cdr ls) seen res return))
     ((member (car (car ls)) res)
      (visit (cdr ls) seen res return))
     (else
      (let scan-deps ((deps (cdr (car ls)))
                      (seen (cons (car (car ls)) seen))
                      (res res))
        (cond
         ((null? deps)
          (visit (cdr ls) seen (cons (car (car ls)) res) return))
         ((member (car deps) seen)
          (scan-deps (cdr deps) seen res))
         ((member (car deps) res)
          (scan-deps (cdr deps) seen res))
         ((assoc (car deps) graph)
          => (lambda (vertices)
               (visit (list vertices)
                      seen
                      res
                      (lambda (seen res)
                        (scan-deps (cdr deps) seen res)))))
         (else
          (scan-deps (cdr deps) seen (cons (car deps) res)))))))))

;; Optimized to avoid redundant computations
(define (rank-votes votes)
  (let* ((tallied (tally-votes votes))
         (sorted (sort-pairs tallied))
         (pairs (map-parallel car sorted))
         (locked (lock-pairs pairs)))
    (topological-sort locked)))

;; Simple assertion helper
(define (assert-equal expected actual message)
  (if (equal? expected actual)
      (begin
        (display "PASS: ")
        (display message)
        (newline))
      (begin
        (display "FAIL: ")
        (display message)
        (display " - Expected: ")
        (display expected)
        (display " Actual: ")
        (display actual)
        (newline))))

;; Function to run test cases
(define (test-rank-votes)
  ;; Test case 1: sample votes from SampleVoteYakHandler.html (Yak Handler example)
  (let* ((votes1
         '((member-a (A) (A-LITE) (SRFI) (B) (R5RS))
           (member-b (B) (SRFI) (R5RS) (A-LITE) (A))
           (member-c (SRFI) (A-LITE) (R5RS) (A B))
           (member-d (R5RS) (A B SRFI A-LITE))
           (member-e (A-LITE) (B) (A) (SRFI) (R5RS))
           (member-f (A) (B) (A-LITE) (SRFI) (R5RS))
           (member-g (B) (A-LITE) (A) (SRFI) (R5RS))
           (member-h (A-LITE) (SRFI) (B) (A) (R5RS))
           ))
         (expected-ranking '(A-LITE B A SRFI R5RS))
         (result (rank-votes votes1)))
    (display "Test case 1 result (Yak Handler example): ")
    (display result)
    (newline)
    (assert-equal expected-ranking result 
                  "Yak Handler vote ranking should match the documented expected outcome"))
  
  ;; Test case 2
  (let ((votes2
         '((member-a (V5) (V6) (V3) (V4) (V1) (V2))
           (member-b (V3) (V6) (V1) (V2) (V5) (V4))
           (member-c (V4) (V2) (V6) (V1) (V5) (V3))
           (member-d (V1) (V5) (V6) (V3) (V4) (V2))
           (member-e (V3) (V1) (V2) (V5) (V6) (V4))
           (member-f (V2) (V6) (V1) (V4) (V3) (V5))
           (member-g (V1) (V3) (V5) (V6) (V4) (V2))
           (member-h (V3) (V5) (V6) (V4) (V2) (V1))
           )))
    (display "Test case 2 result: ")
    (display (rank-votes votes2))
    (newline)))
)
