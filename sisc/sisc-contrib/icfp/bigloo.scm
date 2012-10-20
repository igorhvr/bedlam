(module niceguy
    (include "constants.scm")
    (include "world.scm")
    (include "gamestate.scm")
    (include "seen.scm")
    (include "package.scm")
    (include "bid.scm")
    (include "io.scm")
    (include "weights.scm")
    (include "ai.scm")
    (include "mainloop.scm")
    (include "init.scm"))

(define (void) (if #f #f))

(define-syntax define-alias
  (syntax-rules ()
    ((_ name (args ...) body ...)
     (define-syntax name
       (syntax-rules ()
	 ((_ args ...)
	  body ...))))))

(define (hashtable/get h i . v)
  (let ((r (hashtable-get h i)))
    (if r r (car v))))

(define hashtable/put! hashtable-put!)
(define hashtable/remove! hashtable-remove!)
(define (hashtable/get! hash key thunk)
  (let ((r (hashtable/get hash key)))
    (if r r
        (let ((r (thunk)))
          (hashtable/put! hash key r)
          r))))

(define (hashtable/for-each hash fun)
  (hashtable-for-each fun hash))
(define (hashtable/map fun hash)
  (apply map (cons fun (hashtable->list hash))))

(define open-tcp-socket make-client-socket)
(define open-socket-input-port socket-input)
(define open-socket-output-port socket-output)
(define make-rectangular list)
;(define call/cc call-with-current-continuation)