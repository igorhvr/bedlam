;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(define-java-classes <java.util.concurrent.linked-blocking-queue> <java.util.concurrent.array-blocking-queue>)

(define-generic-java-methods put take size peek)

 ;; Specification:
 ;; (if capacity (j "new java.util.concurrent.ArrayBlockingQueue(capacity);" `((capacity ,(->jint capacity)))) (j "new java.util.concurrent.LinkedBlockingQueue();"))
(define-syntax get-native-blocking-queue
  (syntax-rules ()
    ((_ capacity)
       (if capacity
           (java-new <java.util.concurrent.array-blocking-queue> (->jint capacity))
           (java-new <java.util.concurrent.linked-blocking-queue>)))))

(define (create-queue-with-strings-input-port q)
  (define stored (make-parameter* ""))
  (let* ((read (lambda (port)
                 (if (> (string-length (stored)) 0)
                     (let ((first-char (string-ref (stored) 0)))
                       (stored (string-drop (stored) 1))
                       (char->integer first-char))
                     (let loop ((data (q 'take)))
                       (if (eof-object? data) -1
                           (cond [(= (string-length data) 1)
                                  (char->integer (string-ref data 0))]
                                 [(>= (string-length data) 2)
                                  (stored (string-drop data 1))
                                  (char->integer (string-ref data 0))]
                                 [(= (string-length data) 0)
                                  (loop (q 'take))]))))))
         (read-string (lambda (port mutable-string offset count)
                        (let ((my-char (read port)))
                          (if (= my-char -1)
                              -1
                              (begin
                                (string-set! mutable-string 0 (integer->char my-char))
                                1)))))
         (ready? (lambda (port) (or (not (eqv? 'empty (q 'peek)))
                               (> (length (stored)) 0))))
         (close (lambda (port) (void))))
    (make-custom-character-input-port read read-string ready? close)))


(define make-queue
  (lambda* ((capacity #f))
    (let* ((inner-queue
           (get-native-blocking-queue capacity))
           (paused #f)
           (output-port-promise
            (delay (open-character-output-port
                                (->binary-output-port
                                 (j "new iu.QueueOutputStream(mqueue,null,0);"
                                    `((mqueue ,inner-queue)))))))
           (input-port-parameter (make-parameter* #f)))
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
                              [('poll-java return-when-empty)
                               (if (not paused)
                                   (let ((result (j "q.poll();"  `((q ,inner-queue)))))
                                     (if (java-null? result) return-when-empty result))
                                   return-when-empty)
                               ]
                              [('poll-java)
                               (new-function 'poll-java 'empty)]
                              [('clear)
                               (j "q.clear();"  `((q ,inner-queue)))]
                              [('pause)
                               (set! paused #t) (void)]
                              [('resume)
                               (set! paused #f) (void)]
                              [('size)
                                (->number (size inner-queue))]
                              [('peek return-when-empty)
                               (if (not paused)
                                   (let ((result (peek inner-queue)))
                                     (if (java-null? result) return-when-empty (java-unwrap result)))
                                   return-when-empty)]
                              [('peek)
                               (new-function 'peek 'empty)]
                              [('inner-queue)
                               inner-queue]
                              [('put-scm-lambda)
                               (lambda (scm-object)
                                 (put inner-queue (java-wrap scm-object)))
                               ]
                              [('output-port)
                               (force output-port-promise)]
                              [('input-port)
                               (force output-port-promise)
                               (if (input-port-parameter)
                                   (input-port-parameter)
                                   (let ((new-input-port (create-queue-with-strings-input-port new-function)))
                                     (input-port-parameter new-input-port)
                                     new-input-port))]
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


(define put-log-trace (make-queue))

(define start-worker
  (lambda* (processor work-queue (continue-forever: continue-forever #t) (inner-queue-forced: inner-queue-forced #f) (log-trace-execution: log-trace-execution (make-parameter* #t)) (thread-name: thread-name "work-queue/worker") )
      (watched-thread/spawn 'thread-name: thread-name
       (lambda ()
         (let ((n (get-next-worker-n)))
           (when (log-trace-execution) (put-log-trace 'put (list 'work-queue "Starting worker" n)))
           ((if inner-queue-forced process-all-work-forced process-all-work)
            (lambda (v)
              (when (log-trace-execution) (put-log-trace 'put (list 'work-queue n "Starting  work unit" v)))
              (let ((timings (time (processor v))))
                (when (log-trace-execution) (log-trace 'work-queue n "Completed work unit" v " in " (cdr timings)))
                #t
                ))       
            work-queue
            continue-forever)
           (when (log-trace-execution) (put-log-trace 'put (list "\nStopping worker [w" n "] - nothing else to do.\n")))
           )))))
