;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.
;;; Irregex itself is under its own license - see the irregex directory for more information.

(require-extension (srfi 43)) ; vector-copy!

;; irregex
(load (string-append (iasylum-bedlam-location) "iasylum/irregex/irregex-0.9.2/irregex.scc"))
(load (string-append (iasylum-bedlam-location) "iasylum/irregex/irregex-0.9.2/irregex-utils.scc"))

;; A chunker that works on scheme ports. It can be used as:
;; ---> (irregex-fold/chunked sre kons '() (port-chunker) (list port #f #f))
(define (port-chunker)
  (let ((get-next (lambda (p)
                    (match-let (((port char-num next-chunk) p))
                               (or next-chunk
                                   (if (not (eof-object? (peek-char port)))
                                       (let ((result (list port (read-char port) #f)))
                                         (set-car! (list-tail p 2) result)
                                         result)
                                       #f)))))
        (get-string (lambda (p)
                      (match-let (((port ch next-chunk) p))
                                 (if ch (string ch) "")))))
    (make-irregex-chunker get-next get-string)))
  
