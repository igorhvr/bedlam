;; Port of PLT password module.

;(import debugging)
;(trace 'make-passwords)
;(load "/home/igorhvr/idm/dupemgr/app/WEB-INF/scm/iasylum/password-code.scm")

(import java-io)

(define port->string-list
  (lambda (p)
    (let ((p (j "new java.io.BufferedReader(r);" `((r ,(->jreader p))))))
      (define do (lambda ()
                   (let ((dt (j "p.readLine();" `((p ,p)))))
                     (if (java-null? dt) '()
                         (cons (->string dt) (do))))))
      (do))))


(define choose/uniform
  (lambda (vec)
    (let ( (n (random (vector-length vec)) ) )
      (vector-ref vec n))))

(define min-length 16)
(define max-length 20)

  ;; read-words : path -> (vector-of string)
(define read-words-f
  (lambda (path)
    (list->vector (port->string-list (open-input-file path)))))

  ;; choose-word : (vector-of string) -> string
(define choose-word
  (lambda (words)
    (choose/uniform words)))

(define make-password
    (case-lambda
      [()
       (let ((dict1 "/usr/share/dict/words")
             (dict2 "words"))
         (if (file-exists? dict1)
             (make-password dict1)
             (if (file-exists? dict2)
                 (make-password dict2)
                 (error "Cannot find a file containing a list of words"))))]
      ((path)
       (let ((words (read-words path)))
         (string->password (choose-word words))))))


  ;; correct-length : string -> string
(define correct-length
  (lambda (input)
                 (define pad
                   (lambda (input offset pad-length)
                     (let ((pad-string (make-string pad-length #\0))
                           (chooser
                            (lambda ()
                              (choose/uniform
                               (list->vector (map (lambda (v) (string-ref (number->string v) 0)) (iota 10)))
                               ))))
                       (let loop ((i 0))
                         (if (< i pad-length)
                             (begin
                               (string-set! pad-string i (chooser))
                               (loop (+ i 1)))))
                       (string-append input pad-string))))
    (let ((len (string-length input)))
      (cond
       ((> len max-length)
        (substring input 0 max-length))
       ((< len min-length)
        (pad input len (- min-length len)))
       (else input)))))
; (trace 'correct-length)

  ;; string->password : string -> string
(define string->password
  (lambda (input)
    (let ((input (correct-length input)))
      (add-noise input))))

  ;; add-noise : string -> string
  ;;
  ;; Increase the space of possible passwords by randomly
  ;; manipulating the input string
(define add-noise
  (lambda (input)
    (string-map
     (lambda (char)
       ((choose/uniform (vector char-upcase char-downcase ;char->leet
                                )) char))
     input)))

; To ensure we  don't waste time reloading the same word.
(define read-words)

(set! read-words (memoize read-words-f))
