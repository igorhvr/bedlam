;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/clojure))

(module iasylum/incanter
  (empty-plot
   plot-numbers
   view-numbers
   view-sql)

  (define (empty-plot)
    (clj "(use '(incanter core charts pdf stats io datasets))")
    (clj "(use '(incanter core charts pdf stats io datasets))
          (def zero '(0))
          (def sp (xy-plot zero zero :title \"Graph\" :x-label \"X\" :y-label \"Y\" :series-label \"Origin\" :legend true :points true ))
          sp"))

  (define (plot-numbers . lists)
    (let* ((sp (begin
                 (clj "(use '(incanter core charts pdf stats io datasets))")
                 (clj "(use '(incanter core charts pdf stats io datasets))
                     (def zero '(0))
                     (def sp (xy-plot zero zero :title \"Graph\" :x-label \"X\" :y-label \"Y\" :series-label \"Origin\" :legend true :points true ))
                     sp")))
           (elements (fold (lambda (l max-len) (max max-len (length l))) 0 lists)))
      (pam lists
           (lambda (l)
             (match-let ( ( (x y) (match l
                                         [((xa ya) ...) (list xa                 ya)]
                                         [((tvi) ...)   (list (iota (length tvi)) tvi)]
                                         [(tv ...)     (list (iota (length tv)) tv)]
                                         [whatever    (begin
                                                        (cons '(1 2 3) '(1 2 3)))]
                                         ) ) )
                        (clj "(def x (into [] xl))(def y (into [] c1))(add-lines sp x y :points true)"
                             `((xl ,(jlist->jarray (->jobject x)))
                               (c1 ,(jlist->jarray (->jobject y)))
                               (elements ,(->jobject elements)))))))
      sp))
  
  (define (view-numbers . lists)
    (let ((plot (apply plot-numbers lists)))
      (clj "(view sp)" `((sp ,plot)))
      #t))
  
  (define view-sql
    (lambda*
     (conn-retriever sql (vars #f))
     (let ((tdata (get-data (conn-retriever) sql vars)))
       (log-trace "Will view data stored under " (iasylum-write-string (save-to-somewhere tdata)) " ... ")
       (view-numbers
        (cdr tdata)))))
  )

