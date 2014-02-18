;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define-java-classes <java.util.concurrent.linked-blocking-queue> <java.util.concurrent.array-blocking-queue>)

(define-generic-java-methods put take)

 ;; Specification:
 ;; (if capacity (j "new java.util.concurrent.ArrayBlockingQueue(capacity);" `((capacity ,(->jint capacity)))) (j "new java.util.concurrent.LinkedBlockingQueue();"))
(define-syntax get-native-blocking-queue
  (syntax-rules ()
    ((_ capacity)
       (if capacity
           (java-new <java.util.concurrent.array-blocking-queue> (->jint capacity))
           (java-new <java.util.concurrent.linked-blocking-queue>)))))

(define make-queue
  (lambda* ((capacity #f))
    (let ((inner-queue
           (get-native-blocking-queue capacity))
          (paused #f))
      (letrec ((new-function (match-lambda*                              
                              [('put-scm v)
                               ;; Spec (j "q.put(v);"  `((q ,inner-queue) (v ,(java-wrap v))))
                               (put inner-queue (java-wrap (v)))
                               ]
                              [('take-force) 
                               (java-unwrap (take inner-queue))
                               ]
                              [('put v)
                               ;; Spec (j "q.put(v);"  `((q ,inner-queue) (v ,(java-wrap v))))
                               (put inner-queue (if (java-object? v) v (java-wrap v)))
                               ]
                              [('take) 
                               (if (not paused)
                                   (let ((result (j "q.take();"  `((q ,inner-queue)))))
                                     (java-unwrap result))
                                   'paused)
                               ]
                              [('put-java v)
                               ;; Spec (j "q.put(v);"  `((q ,inner-queue) (v ,(java-wrap v))))
                               (put inner-queue v)
                               ]
                              [('take-java) 
                               (if (not paused)
                                   (let ((result (j "q.take();"  `((q ,inner-queue)))))
                                     result)
                                   'paused)
                               ]
                              [('take return-when-paused)
                               (if (not paused)
                                   (let ((result (j "q.take();"  `((q ,inner-queue)))))
                                     (java-unwrap result))
                                   return-when-paused)
                               ]
                              [('poll return-when-empty)
                               (if (not paused)
                                   (let ((result (j "q.poll();"  `((q ,inner-queue)))))
                                     (if (java-null? result) return-when-empty (java-unwrap result)))
                                   return-when-empty)
                               ]
                              [('poll)
                               (new-function 'poll 'empty)]
                              [('clear)
                               (j "q.clear();"  `((q ,inner-queue)))]
                              [('pause)
                               (set! paused #t) (void)]
                              [('resume)
                               (set! paused #f) (void)]
                              [('inner-queue)
                               inner-queue]
                              )))
        new-function))))

(define (process-all-work proc queue continue-forever)
  (if continue-forever
      (let r ()
        (proc (queue 'take))
        (r))
      (let ((v (queue 'poll)))
        (if (not (eqv? v 'empty)) (begin (proc v) (process-all-work proc queue continue-forever))))))

(define (process-all-work-forced proc queue ignored)
  (let ((q (queue 'inner-queue)))
    (let r ()
      (proc (take q))
      (r))))

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
  (lambda* (processor work-queue (continue-forever #f) (forced #f))
      (thread/spawn
       (lambda ()
         (with-failure-continuation
          (lambda (err cont)
            (d "\nError " err " at " cont "\n"))
          (lambda ()
            (let ((n (get-next-worker-n)))
              (d "\nStarting worker [w" n "]...\n")
              ((if forced process-all-work-forced process-all-work)
               (lambda (v)
                 (define (now) (->string (j "new java.util.Date().toString();")))
                 ;;(d "\n   [w" n "] starting  work unit: " v " (now: " (now) ")\n")
                 (let ((timings (time (processor v))))
                   ;;(d "\n   [w" n "] completed work unit: " v " in " (cdr timings) "\n")
                   #t
                   ))       
               work-queue
               continue-forever)
              (d "\nStopping worker [w" n "] - nothing else to do.\n"))))))))
