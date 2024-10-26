;; JSON implementation for Scheme
;; See http://www.json.org/ or http://www.crockford.com/JSON/index.html
;;
;; Copyright (c) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; JSON Structures are represented as vectors: #((symbol . value) (symbol . value) ...)
;; JSON Arrays are lists
;;


  (define (hashtable->vector ht)
    (list->vector (hash-table->alist ht)) )

  (define json-write
    (let ()
      (define (write-ht vec p)
	(display "{" p)
	(do ((need-comma #f #t)
	     (i 0 (+ i 1)))
	    ((= i (vector-length vec)))
	  (if need-comma
	      (display ", " p)
	      (set! need-comma #t))
	  (let* ((entry (vector-ref vec i))
		 (k (car entry))
		 (v (cdr entry)))
	    (cond
	     ((symbol? k) (write (symbol->string k) p))
	     ((string? k) (write k p)) ;; for convenience
	     (else (error "Invalid JSON table key in json-write" k)))
	    (display ": " p)
	    (write-any v p)))
	(display "}" p))

      (define (write-array a p)
	(display "[" p)
	(let ((need-comma #f))
	  (for-each (lambda (v)
		      (if need-comma
			  (display ", " p)
			  (set! need-comma #t))
		      (write-any v p))
		    a))
	(display "]" p))

      (define (write-any x p)
	(cond
	 ((hash-table? x) (write-ht (hashtable->vector x) p))
	 ((vector? x) (write-ht x p))
	 ((list? x) (write-array x p))
	 ((symbol? x) (write (symbol->string x) p)) ;; for convenience
	 ((string? x) (write x p))
         ((number? x)
          (let ((to-write
                 (if (and (exact? x) (= 1 (denominator x)))
                     x
                     (exact->inexact x))))
            (write to-write p)))
	 ((boolean? x) (display (if x "true" "false") p))
         ((date? x) (write (date->string x "~4") p))
         ((time? x) (write (date->string (time-utc->date x) "~4") p))
	 ((eq? x (void)) (display "null" p))
	 (else (error "Invalid JSON object in json-write" x))))

      (lambda (x . maybe-port)
	(write-any x (if (pair? maybe-port) (car maybe-port) (current-output-port))))))

  (define json-read
    (let ()
      (define (generator p)
	(let ((ateof #f)
	      (pos (top-parse-position "<?>")))
	  (lambda ()
	    (if ateof
		(values pos #f)
		(let ((x (read-char p)))
		  (if (eof-object? x)
		      (begin
			(set! ateof #t)
			(values pos #f))
		      (let ((old-pos pos))
			(set! pos (update-parse-position pos x))
			(values old-pos (cons x x)))))))))

      (define parser
	(packrat-parser (begin
			  (define (white results)
			    (if (char-whitespace? (parse-results-token-value results))
				(white (parse-results-next results))
				(comment results)))
			  (define (skip-comment-char results)
			    (comment-body (parse-results-next results)))
			  (define (skip-to-newline results)
			    (if (memv (parse-results-token-value results) '(#\newline #\return))
				(white results)
				(skip-to-newline (parse-results-next results))))
			  (define (token str)
			    (lambda (starting-results)
			      (let loop ((pos 0) (results starting-results))
				(if (= pos (string-length str))
				    (make-result str results)
				    (if (char=? (parse-results-token-value results) (string-ref str pos))
					(loop (+ pos 1) (parse-results-next results))
					(make-expected-result (parse-results-position starting-results) str))))))
			  (define (interpret-string-escape results k)
			    (let ((ch (parse-results-token-value results))
                                  (results (parse-results-next results)))
                              (if (char=? ch '#\u)
                                  (do ((i 0 (+ i 1)) (str "\"\\u")
                                       (results results (parse-results-next results)))
                                      ((= i 4)
                                       (k
                                        (with-input-from-string (string-append str "\"") read)
                                        results))
                                    (set! str (string-append
                                               str (string (parse-results-token-value results)))))
                                  (k (cond
                                      ((assv ch '((#\b . #\backspace)
                                                  (#\n . #\newline)
                                                  (#\f . #\page)
                                                  (#\r . #\return)
                                                  (#\t . #\tab))) => cdr)
                                      (else ch))
                                     results))))
			  (define (jstring-body results)
			    (let loop ((acc '()) (results results))
			      (let ((ch (parse-results-token-value results)))
				(case ch
				  ((#\\) (interpret-string-escape
                                          (parse-results-next results)
                                          (lambda (val results)
                                            (define new-acc
                                              (if (char? val) (cons val acc)
                                                  (append-reverse! (string->list val) acc)))
                                            (loop new-acc results))))
				  ((#\") (make-result (list->string (reverse acc)) results))
				  (else (loop (cons ch acc) (parse-results-next results)))))))
			  (define (jnumber-body starting-results)
			    (let loop ((acc '()) (results starting-results))
			      (let ((ch (parse-results-token-value results)))
				(if (memv ch '(#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E #\/))
				    (loop (cons ch acc) (parse-results-next results))
				    (let ((n (string->number (decimal-to-fractions-inside-string
                                                              (list->string (reverse acc))))))
				      (if n
					  (make-result n results)
					  (make-expected-result (parse-results-position starting-results) 'number)))))))
			  any)
			(any ((white '#\{ entries <- table-entries white '#\}) (list->vector entries))
			     ((white '#\[ entries <- array-entries white '#\]) entries)
			     ((s <- jstring) s)
			     ((n <- jnumber) n)
			     ((white (token "true")) #t)
			     ((white (token "false")) #f)
			     ((white (token "null")) (void)))
			(comment (((token "/*") b <- comment-body) b)
				 (((token "//") b <- skip-to-newline) b)
				 (() 'whitespace))
			(comment-body (((token "*/") w <- white) w)
				      ((skip-comment-char) 'skipped-comment-char))
			(table-entries ((a <- table-entries-nonempty) a)
				       (() '()))
			(table-entries-nonempty ((entry <- table-entry white '#\, entries <- table-entries-nonempty) (cons entry entries))
						((entry <- table-entry) (list entry)))
			(table-entry ((key <- jstring white '#\: val <- any) (cons key val)))
			(array-entries ((a <- array-entries-nonempty) a)
				       (() '()))
			(array-entries-nonempty ((entry <- any white '#\, entries <- array-entries-nonempty) (cons entry entries))
						((entry <- any) (list entry)))
			(jstring ((white '#\" body <- jstring-body '#\") body))
			(jnumber ((white body <- jnumber-body) body))
			))

      (define (read-any p)
	(let ((result (parser (base-generator->results (generator p)))))
	  (if (parse-result-successful? result)
	      (parse-result-semantic-value result)
	      (error "JSON Parse Error"
		     (let ((e (parse-result-error result)))
		       (list 'json-parse-error
			     (parse-position->string (parse-error-position e))
			     (parse-error-expected e)
			     (parse-error-messages e)))))))

      (lambda maybe-port
	(read-any (if (pair? maybe-port) (car maybe-port) (current-input-port))))))

(define (scheme->json structure)
  (call-with-output-string
   (lambda (output-port)
     (json-write
      structure
      output-port))))
  
(define (json->scheme string)
  (call-with-input-string
   string
   (lambda (input-port)
     (json-read input-port))))

(define (beautify-json st) (->string (j "com.cedarsoftware.util.io.JsonWriter.formatJson(jsonst);" `((jsonst ,(->jstring st))))))

(define (json->sxml e)
  (let ((structure (if (string? e) (json->scheme e) e)))
    `(*TOP*
      ,@(json->sxml-block structure))))

(define (json->sxml-block structure)
  (match structure
         ( #( ( (? string? a) . b ) ...) (=> fail)
           (map (lambda (a b) (cond ( (or (string? b) (number? b) (boolean? b))  `(,(string->symbol a)  ,b))
                               ( (void? b)                                  `(,(string->symbol a)    ))
                               ( (vector? b)                                `(,(string->symbol a) . ,(json->sxml-block b)))
                               (else (fail))))
                a b))
         
         ( whatever (throw (make-error (string-append* "Support not yet implemented in json->sxml-block for structure: ===" whatever "=== ."))))))

(define* (sort-json-object-by-keys json-object (return-parsed-object: return-parsed-object #f) (deeply: deeply #f))
  (let ((params-scheme (if (string? json-object) (json->scheme json-object) json-object)))
    (match params-scheme
           [#(((? string? json-parameter) . parameter) ...)
            (let* ((list-result (sort (lambda (e1 e2)
                                        (string< (car e1) (car e2)))
                                      (let ((possibly-recursively-sorted-parameter
                                             (map (lambda (v)
                                                  (if (and deeply (vector? v))
                                                      (sort-json-object-by-keys v
                                                                                'return-parsed-object: #t
                                                                                'deeply: deeply)
                                                      v))
                                                parameter)))
                                      (map cons
                                           json-parameter
                                           possibly-recursively-sorted-parameter))))
                   (vector-result (list->vector list-result)))
              (if return-parsed-object vector-result (scheme->json vector-result)))]
           [else
            (throw (make-error "sort-json-object-by-keys received something which is not a json object with string keys to sort."))]
           )))

(define* (add-missing-properties-to-json-object
          json-object additional-properties
          (return-parsed-object: return-parsed-object #f) (sort-result: sort-result #t))
  (let* ((compare-param-keys (lambda (e1 e2) (string=? (car e1) (car e2))))
         (params-scheme (if (string? json-object) (json->scheme json-object) json-object)))
    (let ((list-result 
           (apply lset-adjoin (append (list compare-param-keys (vector->list params-scheme))
                                      (vector->list (if (string? additional-properties)
                                                        (json->scheme additional-properties)
                                                        additional-properties))))))
      (let ((unsorted-vector-result (list->vector list-result)))
        (let ((as-requested-vector-result
               (if sort-result
                   (sort-json-object-by-keys unsorted-vector-result 'return-parsed-object: #t)
                   unsorted-vector-result)))
          (if return-parsed-object
              as-requested-vector-result
              (scheme->json as-requested-vector-result)))))))

(define (test-add-missing-properties-to-json-object)
  (d/n 
   (add-missing-properties-to-json-object (json->scheme "{\"xyz\" : false, \"deposit_info\": \"{\\\"address\\\": \\\"6401691a-2106-4486-b723-9a3c598c37c5\\\", \\\"currency\\\": \\\"brl\\\", \\\"receipt\\\": \\\"CANCELAR_DEPOSITO_TESTE\\\", \\\"sourceBankId\\\": \\\"001\\\", \\\"destinationBankId\\\": \\\"001\\\"}\", \"declared_value\": \"99/100\"}") (json->scheme "{\"xy\" : 13, \"deposit_info\": \"{\\\"address\\\": \\\"6401691a-2106-4486-b723-9a3c598c37c5\\\", \\\"currency\\\": \\\"brl\\\", \\\"receipt\\\": \\\"CANCELAR_DEPOSITO_TESTE\\\", \\\"sourceBankId\\\": \\\"001\\\", \\\"destinationBankId\\\": \\\"001\\\"}\", \"declared_value\": \"99/100\"}") 'return-parsed-object: #t 'sort-result: #t))
  (throw (make-error "TODO: Add assertion. For now verify manually above.")))

;; This allows us to transform a list with a single header line into a JSON object.
;;
;; Sample usage:
;; (let* ((ss (excel-spreadsheet->list "/tmp/spreadsheet.xls"))(sheet (car ss)))(d/n (header-and-data-list->json sheet 'value-to-consider-blank: 'blank)))
(define* (header-and-data-list->json p
                                     (value-to-consider-blank: value-to-consider-blank #f)
                                     (fill-blanks: fill-blanks #f)
                                     (fill-blanks-with: blank-value #f)
                                     (should-beautify-json: should-beautify-json #t)
                                     (return-object: return-object #f))
  (when (not fill-blanks) (assert (not blank-value)))

  (let ((result-object
         (let* ((column-names (list->vector (car p)))
                (data (cdr p))
                (data-vector (list->vector data)))
           (list-ec (: data-index 0 (vector-length data-vector))
                    (let ((current-row (list-ref data data-index)))
                      (list->vector (filter
                                     (match-lambda
                                      ((key . value)
                                       (if (not fill-blanks) value #t )))
                                     (map
                                      (lambda (label-position value)
                                        (cons (vector-ref column-names label-position)
                                              (or (and (not (eqv? value-to-consider-blank value)) value) blank-value)))
                                      (list-ec (: i 0 (vector-length column-names)) i)
                                      (append current-row (map (lambda (ignored)
                                                                 value-to-consider-blank)
                                                               (iota (- (vector-length column-names) (length current-row)))))))))))))
    (if return-object result-object
        (let ((string-result (scheme->json result-object)))
          (if should-beautify-json
              (beautify-json string-result)
              string-result)))))
