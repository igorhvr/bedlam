(define seen-table (make-hashtable))


(define (sseen? moves pos)
  (cond [(null? moves) 0]
        [(null? (node-parent moves)) 0]
        [(equal? (node-pos moves) pos)
         (+ 1 (sseen? (node-parent moves) pos))]
        [else (sseen? (node-parent moves) pos)]))


(define (seen? id x y) 
  (let ((seenworld (hashtable/get! seen-table id (lambda () (make-string 
							     (* world-width world-height))))))
							     
    (char->integer (string-ref seenworld (world-idx x y)))))

(define (unseen! id pos)
  (let ((seenworld (hashtable/get! seen-table id (lambda () (make-string 
							     (* world-width world-height))))))
    (string-set! seenworld (world-idx x y) (integer->char 
                                            (- 1 (char->integer 
                                                  (string-ref seenworld 
                                                              (world-idx x y))))))))

(define (clear-seen! id)
  (hashtable/remove! seen-table id))

(define (set-seen! id x y)
  (let ((seenworld (hashtable/get! seen-table id (lambda () (make-string 
							     (* world-width world-height))))))
    (string-set! seenworld (world-idx x y) 
                 (integer->char
                  (+ (char->integer (string-ref seenworld (world-idx x y)))
                     1)))))
                    
                    

