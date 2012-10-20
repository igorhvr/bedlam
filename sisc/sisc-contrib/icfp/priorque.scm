;;; Checks to see if a list has any duplicate MEMBERs.
(define (comlist:has-duplicates? lst)
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (comlist:has-duplicates? (cdr lst)))))
(define (comlist:every pred lst . rest)
  (cond ((null? rest)
	 (let mapf ((lst lst))
	   (or (null? lst)
	       (and (pred (car lst)) (mapf (cdr lst))))))
	(else (let mapf ((lst lst) (rest rest))
		(or (null? lst)
		    (and (apply pred (car lst) (map car rest))
			 (mapf (cdr lst) (map cdr rest))))))))
(define (comlist:notevery pred . ls) (not (apply comlist:every pred ls)))

(define (comlist:position obj lst)
  (letrec ((pos (lambda (n lst)
		  (cond ((null? lst) #f)
			((eqv? obj (car lst)) n)
			(else (pos (+ 1 n) (cdr lst)))))))
    (pos 0 lst)))

; "record.scm" record data types
; Written by David Carlton, carlton@husc.harvard.edu.
; Re-Written by Aubrey Jaffer, agj @ alum.mit.edu, 1996, 1997
;
; This code is in the public domain.

; Implements `record' data structures for Scheme.  Using only the
; opacity of procedures, makes record datatypes and
; record-type-descriptors disjoint from R4RS types and each other, and
; prevents forgery and corruption (modification without using
; RECORD-MODIFIER) of records.

;;2001-07-24  Aubrey Jaffer  <agj@alum.mit.edu>
;;  changed identifiers containing VECTOR to VECT or VCT.

(define vector? vector?)
(define vector-ref vector-ref)
(define vector-set! vector-set!)
(define vector-fill! vector-fill!)
(define vector->list vector->list)
(define display display)
(define write write)

(define record-modifier #f)
(define record-accessor #f)
(define record-constructor #f)
(define record-predicate #f)
(define make-record-type #f)

(let (;; Need to close these to keep magic-cookie hidden.
      (make-vect make-vector)
      (vect vector)

      ;; We have to wrap these to keep magic-cookie hidden.
      (vect? vector?)
      (vect-ref vector-ref)
      (vect->list vector->list)
      (disp display)
      (wri write)

      ;; Need to wrap these to protect record data from being corrupted.
      (vect-set! vector-set!)
      (vect-fill! vector-fill!)

      (nvt "of non-vector type")
      )
  (letrec
      (;; Tag to identify rtd's.  (A record is identified by the rtd
       ;; that begins it.)
       (magic-cookie (cons 'rtd '()))
       (rtd? (lambda (object)
	       (and (vect? object)
		    (not (= (vector-length object) 0))
		    (eq? (rtd-tag object) magic-cookie))))
       (rec? (lambda (obj)
	       (and (vect? obj)
		    (>= (vector-length obj) 1)
		    (or (eq? magic-cookie (rec-rtd obj))
			(rtd? (rec-rtd obj))))))

       (vec:error
	(lambda (proc-name msg obj)
	  (error proc-name msg
		      (cond ((rtd? obj) 'rtd)
			    ((rec? obj) (rtd-name (rec-rtd obj)))
			    (else obj)))))

       ;; Internal accessor functions.  No error checking.
       (rtd-tag (lambda (x) (vect-ref x 0)))
       (rtd-name (lambda (rtd) (if (vect? rtd) (vect-ref rtd 1) "rtd")))
       (rtd-fields (lambda (rtd) (vect-ref rtd 3)))
       ;; rtd-vfields is padded out to the length of the vector, which is 1
       ;; more than the number of fields
       (rtd-vfields (lambda (rtd) (cons #f (rtd-fields rtd))))
       ;; rtd-length is the length of the vector.
       (rtd-length (lambda (rtd) (vect-ref rtd 4)))

       (rec-rtd (lambda (x) (vect-ref x 0)))
       (rec-disp-str
	(lambda (x)
	  (let ((name (rtd-name (rec-rtd x))))
	    (string-append
	     "#<" (if (symbol? name) (symbol->string name) name) ">"))))

       (make-rec-type
	(lambda (type-name field-names)
	  (if (not (or (symbol? type-name) (string? type-name)))
	      (error 'make-record-type "non-string type-name argument."
			  type-name))
	  (if (or (and (list? field-names) (comlist:has-duplicates? field-names))
		  (comlist:notevery symbol? field-names))
	      (error 'make-record-type "illegal field-names argument."
			  field-names))
	  (let* ((augmented-length (+ 1 (length field-names)))
		 (rtd (vect magic-cookie
			    type-name
			    '()
			    field-names
			    augmented-length
			    #f
			    #f)))
	    (vect-set! rtd 5
		       (lambda (x)
			 (and (vect? x)
			      (= (vector-length x) augmented-length)
			      (eq? (rec-rtd x) rtd))))
	    (vect-set! rtd 6
		       (lambda (x)
			 (and (vect? x)
			      (>= (vector-length x) augmented-length)
			      (eq? (rec-rtd x) rtd)
			      #t)))
	    rtd)))

       (rec-predicate
	(lambda (rtd)
	  (if (not (rtd? rtd))
	      (error 'record-predicate "invalid argument." rtd))
	  (vect-ref rtd 5)))

       (rec-constructor
	(lambda (rtd . field-names)
	  (if (not (rtd? rtd))
	      (error 'record-constructor "illegal rtd argument." rtd))
	  (if (or (null? field-names)
		  (equal? field-names (rtd-fields rtd)))
	      (let ((rec-length (- (rtd-length rtd) 1)))
		(lambda elts
		  (if (= (length elts) rec-length) #t
		      (error 'record-constructor
				  (rtd-name rtd)
				  "wrong number of arguments."))
		  (apply vect rtd elts)))
	      (let ((rec-vfields (rtd-vfields rtd))
		    (corrected-rec-length (rtd-length rtd))
		    (field-names (car field-names)))
		(if (or (and (list? field-names) (comlist:has-duplicates? field-names))
			(comlist:notevery (lambda (x) (memq x rec-vfields))
					  field-names))
		    (error
		     'record-constructor "invalid field-names argument."
		     (cdr rec-vfields)))
		(let ((field-length (length field-names))
		      (offsets
		       (map (lambda (field) (comlist:position field rec-vfields))
			    field-names)))
		  (lambda elts
		    (if (= (length elts) field-length) #t
			(error 'record-constructor
				    (rtd-name rtd)
				    "wrong number of arguments."))
		    (let ((result (make-vect corrected-rec-length)))
		      (vect-set! result 0 rtd)
		      (for-each (lambda (offset elt)
				  (vect-set! result offset elt))
				offsets
				elts)
		      result)))))))

       (rec-accessor
	(lambda (rtd field-name)
	  (if (not (rtd? rtd))
	      (error 'record-accessor "invalid rtd argument." rtd))
	  (let ((index (comlist:position field-name (rtd-vfields rtd)))
		(augmented-length (rtd-length rtd)))
	    (if (not index)
		(error 'record-accessor "invalid field-name argument."
			    field-name))
	    (lambda (x)
	      (if (and (vect? x)
		       (>= (vector-length x) augmented-length)
		       (eq? rtd (rec-rtd x)))
		  #t
		  (error 'record-accessor "wrong record type." x "not" rtd))
	      (vect-ref x index)))))

       (rec-modifier
	(lambda (rtd field-name)
	  (if (not (rtd? rtd))
	      (error 'record-modifier "invalid rtd argument." rtd))
	  (let ((index (comlist:position field-name (rtd-vfields rtd)))
		(augmented-length (rtd-length rtd)))
	    (if (not index)
		(error 'record-modifier "invalid field-name argument."
			    field-name))
	    (lambda (x y)
	      (if (and (vect? x)
		       (>= (vector-length x) augmented-length)
		       (eq? rtd (rec-rtd x)))
		  #t
		  (error 'record-modifier "wrong record type." x "not" rtd))
	      (vect-set! x index y)))))
       )
    (set! vector? (lambda (obj) (and (vect? obj) (not (rec? obj)))))
    (set! vector-ref
	  (lambda (vct k)
	    (cond ((rec? vct)
		   (vec:error 'vector-ref nvt vct))
		  (else (vect-ref vct k)))))
    (set! vector->list
	  (lambda (vct)
	    (cond ((rec? vct)
		   (vec:error 'vector->list nvt vct))
		  (else (vect->list vct)))))
    (set! vector-set!
	  (lambda (vct k obj)
	    (cond ((rec? vct) (vec:error 'vector-set! nvt vct))
		  (else (vect-set! vct k obj)))))
    (set! vector-fill!
	  (lambda (vector fill)
	    (cond ((rec? vct)
		   (vec:error 'vector-fill! nvt vct))
		  (else (vect-fill! vct fill)))))
    (set! display
	  (lambda (obj . opt)
	    (apply disp (if (rec? obj) (rec-disp-str obj) obj) opt)))
    (set! write
	  (lambda (obj . opt)
	    (if (rec? obj)
		(apply disp (rec-disp-str obj) opt)
		(apply wri obj opt))))
    (set! record-modifier rec-modifier)
    (set! record-accessor rec-accessor)
    (set! record-constructor rec-constructor)
    (set! record-predicate rec-predicate)
    (set! make-record-type make-rec-type)
    ))

;;;; "priorque.scm" priority queues for Scheme.
;;; Copyright (C) 1992, 1993, 1994, 1995, 1997 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Algorithm from:
;;; Introduction to Algorithms by T. Cormen, C. Leiserson, R. Rivest.
;;; 1989 MIT Press.

;; Record type.
(define heap:rtd (make-record-type "heap" '(array size heap<?)))

;; Constructor.
(define heap:make-heap
  (let ((cstr (record-constructor heap:rtd)))
    (lambda (pred<?)
      (cstr (make-vector 4) 0 pred<?))))

;; Reference an element.
(define heap:ref
  (let ((ra (record-accessor heap:rtd 'array)))
    (lambda (a i)
      (vector-ref (ra a) (+ -1 i)))))

;; Set an element.
(define heap:set!
  (let ((ra (record-accessor heap:rtd 'array)))
    (lambda (a i v)
      (vector-set! (ra a) (+ -1 i) v))))

;; Exchange two elements.
(define heap:exchange
  (let ((aa (record-accessor heap:rtd 'array)))
    (lambda (a i j)
      (set! i (+ -1 i))
      (set! j (+ -1 j))
      (let* ((ra (aa a))
	     (tmp (vector-ref ra i)))
	(vector-set! ra i (vector-ref ra j))
	(vector-set! ra j tmp)))))


;; Get length.
(define heap:length (record-accessor heap:rtd 'size))

(define heap:heap<? (record-accessor heap:rtd 'heap<?))

(define heap:set-size!
  (let ((aa (record-accessor heap:rtd 'array))
	(am (record-modifier heap:rtd 'array))
	(sm (record-modifier heap:rtd 'size)))
    (lambda (a s)
      (let ((ra (aa a)))
	(if (> s (vector-length ra))
	    (let ((nra (make-vector (+ s (quotient s 2)))))
	      (do ((i (+ -1 (vector-length ra)) (+ -1 i)))
		  ((negative? i) (am a nra))
		(vector-set! nra i (vector-ref ra i)))))
	(sm a s)))))

(define (heap:parent i) (quotient i 2))
(define (heap:left i) (* 2 i))
(define (heap:right i) (+ 1 (* 2 i)))

(define (heap:heapify a i)
  (let* ((l (heap:left i))
	 (r (heap:right i))
	 (largest (if (and (<= l (heap:length a))
			   ((heap:heap<? a) (heap:ref a i) (heap:ref a l)))
		      l
		      i)))
    (cond ((and (<= r (heap:length a))
		((heap:heap<? a) (heap:ref a largest) (heap:ref a r)))
	   (set! largest r)))
    (cond ((not (= largest i))
	   (heap:exchange a i largest)
	   (heap:heapify a largest)))))

(define (heap:insert! a key)
  (define i (+ 1 (heap:length a)))
  (heap:set-size! a i)
  (do ()
      ((not (and (> i 1)
		 ((heap:heap<? a) (heap:ref a (heap:parent i)) key))))
    (heap:set! a i (heap:ref a (heap:parent i)))
    (set! i (heap:parent i)))
  (heap:set! a i key))

(define (heap:extract-max! a)
  (if (< (heap:length a) 1)
      (error "heap underflow" a))
  (let ((max (heap:ref a 1)))
    (heap:set! a 1 (heap:ref a (heap:length a)))
    (heap:set-size! a (+ -1 (heap:length a)))
    (heap:heapify a 1)
    max))

;;
;; Externals.
;;
(define make-heap heap:make-heap)
(define heap-insert! heap:insert!)
(define heap-extract-max! heap:extract-max!)
(define heap-length heap:length)

(define (heap:test)
  (require 'debug)
  (let ((heap #f))
    (set! heap (make-heap char>?))
    (heap-insert! heap #\A)
    (heap-insert! heap #\Z)
    (heap-insert! heap #\G)
    (heap-insert! heap #\B)
    (heap-insert! heap #\G)
    (heap-insert! heap #\Q)
    (heap-insert! heap #\S)
    (heap-insert! heap #\R)
    (do ((i 7 (+ -1 i)))
	((negative? i))
      (write (heap-extract-max! heap)) (newline))))
