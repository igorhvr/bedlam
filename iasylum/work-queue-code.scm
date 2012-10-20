;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.


(define make-queue
  (lambda* ((capacity #f))
    (let ((inner-queue
           (if capacity
               (j "new java.util.concurrent.LinkedBlockingQueue(capacity);" `((capacity ,(->jint capacity))))
               ;;(j "new java.util.concurrent.ConcurrentLinkedQueue();"))
               (j "new java.util.concurrent.LinkedBlockingQueue();"))
           )
          (paused #f))
      (letrec ((new-function (match-lambda*
                              [('add v)
                               (if capacity
                                   (j "q.put(v);"  `((q ,inner-queue) (v ,(java-wrap v))))
                                   (j "q.add(v);"  `((q ,inner-queue) (v ,(java-wrap v)))))
                               ]
                              [('put v)
                               (if capacity
                                   (j "q.put(v);"  `((q ,inner-queue) (v ,(java-wrap v))))
                                   (j "q.add(v);"  `((q ,inner-queue) (v ,(java-wrap v)))))
                               ]
                              [('clear)
                               (j "q.clear();"  `((q ,inner-queue)))]
                              [('pause)
                               (set! paused #t) (void)]
                              [('resume)
                               (set! paused #f) (void)]
                              [('take return-when-paused)
                               (if (not paused)
                                   (let ((result (j "q.take();"  `((q ,inner-queue)))))
                                     (java-unwrap result))
                                   return-when-paused)
                               ]
                              [('take)
                               (new-function 'take 'paused)]
                              [('poll return-when-empty)
                               (if (not paused)
                                   (let ((result (j "q.poll();"  `((q ,inner-queue)))))
                                     (if (java-null? result) return-when-empty (java-unwrap result)))
                                   return-when-empty)
                               ]
                              [('poll)
                               (new-function 'poll 'empty)])))
        new-function))))

(define (process-all-work proc queue)
  (let ((v (queue 'poll)))
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

(define start-worker
  (lambda* (processor work-queue (continue-forever #f))
      (thread/spawn
       (lambda ()
         (with-failure-continuation
          (lambda (err cont)
            (d "\nError " err " at " cont "\n"))
          (lambda ()
            (let ((n (get-next-worker-n)))
              (d "\nStarting worker [w" n "]...\n")
              (let r ()
                (process-all-work
                 (lambda (v)
                   (define (now) (->string (j "new java.util.Date().toString();")))
                   ;;(d "\n   [w" n "] starting  work unit: " v " (now: " (now) ")\n")
                   (let ((timings (time (processor v))))
                     #t;;(d "\n   [w" n "] completed work unit: " v " in " (cdr timings) "\n")
                     )
                   )       
                 work-queue)
                (if continue-forever (r)))
              (d "\nStopping worker [w" n "] - nothing else to do.\n"))))))))
