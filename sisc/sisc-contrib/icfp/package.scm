; Package representation

(define new-packages #f)

(define (remove x ls)
  (if (null? ls) '()
      (if (eq? x (car ls))
	  (remove x (cdr ls))
	  (cons (car ls) (remove x (cdr ls))))))

(define (removep x ls)
  (if (null? ls) '()
      (if (= (package-id x) (package-id (car ls)))
	  (removep x (cdr ls))
	  (cons (car ls) (removep x (cdr ls))))))

(define unclaimed-packages (make-hashtable))
(define claimed-packages (make-hashtable))
(define all-packages (make-hashtable))
(define package-locations (make-hashtable))


(define (packages . loc)
  (hashtable/get package-locations (apply make-rectangular loc)
                 '()))
(define (package-at! p pos) 
  (hashtable/put! package-locations (apply make-rectangular pos) 
                  (cons p (removep p (apply packages pos)))))

(define (package-not-at! p pos) 
  (let* ((op (apply packages pos))
         (np (removep p op)))
    (hashtable/put! package-locations (apply make-rectangular pos) 
                    np)))

(define (make-package id)
  (let ((p (make-hashtable)))
    (hashtable/put! p 'id id)
    p))

(define (package-id p) (hashtable/get p 'id))
(define (package-location p) (hashtable/get p 'location))
(define (package-destination p) (hashtable/get p 'destination))
(define (package-weight p) (hashtable/get p 'weight))

(define (package-details! p destx desty weight)
  (hashtable/put! p 'destination (list destx desty))
  (hashtable/put! p 'weight weight))

(define (package-location! p . loc)
  (hashtable/put! p 'location loc))

(define (package-lookup id)
  (hashtable/get! all-packages id (lambda () (set! new-packages #t)
                                          (make-package id))))

; Called when we observe a drop action.  This does nothing if the package
; was delivered (we knew its destination and it was dropped at that destination)
; If it was dropped and we don't know its destination, we record the package as
; unclaimed.
(define (package-drop! p id . droploc)
  (let ((loc (package-destination p)))
    (begin (if (not (and loc (equal? loc droploc)))
               (hashtable/put! unclaimed-packages (package-id p) p)
               (hashtable/remove! all-packages (package-id p)))
           (hashtable/put! 
            claimed-packages id 
            (removep p (hashtable/get claimed-packages id '())))
           (robot-load-decr! id (package-weight p))
           (apply package-location! `(,p ,@droploc))
           (clear-seen! id)))
  droploc)


(define (package-add! p)
  (hashtable/put! unclaimed-packages (package-id p) p)
  (hashtable/put! all-packages (package-id p) p)
  (package-at! p (package-location p)))

                  
    
; Called when we observe a pickup action.  This removes the package
; from those we know about that are unclaimed.
(define (package-pickup! p id . pickuploc)
  (when p
	(hashtable/remove! unclaimed-packages (package-id p))
	(robot-load-incr! id (package-weight p))
	(hashtable/put! claimed-packages id
                        (cons p (hashtable/get claimed-packages id '())))
        (package-not-at! p pickuploc)
        (clear-seen! id)))

(define (robots-packages id)
  (hashtable/get claimed-packages id '()))
