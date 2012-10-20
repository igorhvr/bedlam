; Bidding

(define money (make-hashtable))
(define estimated-bids (make-hashtable))

(define (robot-money id) 
  (hashtable/get money id 0))

(define (robot-money! id v)
  (hashtable/put! money id v))

(define (bid! id v)
  (robot-money! id (- (robot-money id) v)))

(define (forceful-bid id before)
  ((if before + -)
   (hashtable/get! estimated-bids id (lambda () 5))))

(define (peaceful-bid id) 1)

(define (adjust-bid! id success)
  (hashtable/put! estimated-bids id 
		  ((if success + -) (forceful-bid id #t) 1)))
(Trace 'adjust-bid!)

