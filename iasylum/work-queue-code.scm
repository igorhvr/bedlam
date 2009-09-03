;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.


(define make-queue
  (lambda ()
    (let ((inner-queue (j "new java.util.concurrent.ConcurrentLinkedQueue();"))
          (paused #f))
      (letrec ((new-function (match-lambda*
                              [('add v)
                               (j "q.add(v);"  `((q ,inner-queue) (v ,(java-wrap v))))]
                              [('clear)
                               (j "q.clear();"  `((q ,inner-queue)))]
                              [('pause)
                               (set! paused #t) (void)]
                              [('resume)
                               (set! paused #f) (void)]
                              [('pool return-when-empty)
                               (if (not paused)
                                   (let ((result (j "q.poll();"  `((q ,inner-queue)))))
                                     (if (java-null? result) return-when-empty (java-unwrap result)))
                                   return-when-empty)
                               ]
                              [('pool)
                               (new-function 'pool 'empty)])))
        new-function))))

(define (process-all-work proc queue)
  (let ((v (queue 'pool)))
    (if (not (eqv? v 'empty)) (begin (proc v) (process-all-work proc queue)))))

(define get-next-worker-n
  (let ((m (mutex/new))
        (n 0))
    (lambda p
      (mutex/lock! m)
      (let ((result n))
        (set! n (+ n 1))
        (mutex/unlock! m)
        result))))

(define (now) (->string (j "new java.util.Date().toString();")))

(define (start-worker processor work-queue)
  (thread/spawn
   (lambda ()
     (with-failure-continuation
      (lambda (err cont)
	(d "\nError " err " at " cont "\n"))
      (lambda ()
	(let ((n (get-next-worker-n)))
	  (d "\nStarting worker [w" n "]...\n")
	  (process-all-work
	   (lambda (v)
	     (define (now) (->string (j "new java.util.Date().toString();")))
	     (d "\n   [w" n "] starting  work unit: " v " (now: " (now) ")\n")
	     (let ((timings (time (processor v)))) (d "\n   [w" n "] completed work unit: " v " in " (cdr timings) "\n"))
	     )       
	   work-queue)
	  (d "\nStopping worker [w" n "] - nothing else to do.\n")))))))
