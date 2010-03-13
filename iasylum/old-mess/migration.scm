(define scheduling_request_table "dbo.scheduling_request")
(define scheduling_status_history_table "dbo.scheduling_status_history")
(define scheduling_request_table_old "DB0QPTS0.dbo.scheduling_request_old")
(define scheduling_status_history_table_old "
DB0QPTS0.dbo.scheduling_status_history_old")

(display "\n\n === Starting migration. === \n\n")
(import hashtable)
(max-stack-trace-depth 16)
(import s2j)
(import generic-procedures)
(import debugging)
(require-library 'sisc/libs/srfi)

(define-generic-java-method put)
(define-generic-java-method get)

(define <map> (java-class '|java.util.HashMap|))

(define sample-map (java-new <map>))

(define-generic-java-method for-name |forName|)

(define-generic-java-method java-trim |trim|)

(define string-trim
  (lambda (s)
    (->string (java-trim (->jstring s)))))

(define <class> (java-class '|java.lang.Class|))
(for-name (java-null <class>) (->jstring "com.sybase.jdbc2.jdbc.SybDriver"))

(define-generic-java-method get-connection |getConnection|)
(define <driver-manager> (java-class '|java.sql.DriverManager|))

(define prod-connection-string
  "jdbc:sybase:Tds:xxx:xx/xxx/?charset=iso_1")
(define prod-login "xxx")
(define prod-password "xxx")

(define dev-connection-string
  "jdbc:sybase:Tds:ahostsomewhere.com:1234/DBNAME/?charset=iso_1")
(define dev-login "ptstest")
(define dev-password "february")

(define qa-connection-string
  "xxxxx")
(define qa-login "pxt")
(define qa-password "febrxy")

(define-macro set-db
 (lambda (type)
   `(begin
      (define connection-string
        ,(string->symbol (string-append (symbol->string type)
"-connection-string")))
      (define login
        ,(string->symbol (string-append (symbol->string type) "-login")))
      (define password
        ,(string->symbol (string-append (symbol->string type) "-password")))
      )))

;(set-db prod)

; (define new-connection
;  (lambda ()
;    (get-connection (java-null <driver-manager>) (->jstring
connection-string) (->jstring login) (->jstring password))))

(define qa-new-connection
  (lambda ()
    (get-connection (java-null <driver-manager>) (->jstring
qa-connection-string) (->jstring qa-login) (->jstring qa-password))))

(define prod-new-connection
  (lambda ()
    (get-connection (java-null <driver-manager>) (->jstring
prod-connection-string) (->jstring prod-login) (->jstring prod-password))))

(define-generic-java-method create-statement)

(define new-statement
  (lambda (connection)
    (create-statement connection)))



(define n-s
  (lambda ()
    (new-statement (new-connection))))

(define-generic-java-method execute-query)
(define-generic-java-method execute)
(define-generic-java-method get-generated-keys)
(define-generic-java-method get-generated-keys |getGeneratedKeys|)
(define-generic-java-method get-result-set)
(define-generic-java-method execute-update)
(define-generic-java-method next)
(define-generic-java-method get-string)

(define o
 (lambda (table)
   (execute-query (n-s)
    (->jstring
     (string-append "SELECT * from " table)))))


;(define q
; (lambda (query)
;   (display (string-append "\n" query "\n"))
;   (execute-query (n-s)
;    (->jstring query))))

(define qa-q
 (lambda (query)
   (display (string-append "\n" query "\n"))
   (execute-query (new-statement (qa-new-connection))
    (->jstring query))))

(define prod-q
 (lambda (query)
   (display (string-append "\n" query "\n"))
   (execute-query (new-statement (prod-new-connection))
    (->jstring query))))

(define-generic-java-method gmd |getMetaData|)

(define d (lambda (i result-set) (let ((v (get-string result-set (->jint
i))))
   (if (java-null? v) "NULL" (->string v)))))

(define n (lambda (result-set) (next result-set)))

; Remember this for SPs: (define-generic-java-method gmr |getMoreResults|)


(define nl
 (lambda (result-set)
   (let loop ((i 1) (m (column-count)))
     (unless (> i m)
             (begin (display "\n") (display (d i result-set)) (loop (+ i 1)
m))))))

(define-generic-java-method get-column-count |getColumnCount|)

(define column-count (lambda (result-set) (->number (get-column-count (gmd
result-set)))))

(define-generic-java-method get-column-name |getColumnName|)

(define column-name (lambda (i result-set) (->string (get-column-name (gmd
result-set) (->jint i)))))

(define-generic-java-method get-column-type-name |getColumnTypeName|)

(define column-type (lambda (i result-set) (->string (get-column-type-name
(gmd result-set) (->jint i)))))

(define desc
 (lambda (result-set)
   (let l ((i 1) (m (column-count result-set)))
     (begin (display "\n") (display (column-name i result-set))
            (display  ": ")
            (display (d i result-set))
            (unless (= i m) (l (+ i 1) m))))
   (display "\n\n")))

(define-generic-java-method set-max-rows |setMaxRows|)

(define get-column-names
  (lambda (result-set)
    (let l ((i 1) (m (column-count result-set)))
 (if (= i m)
     (list (column-name i result-set))
     (append (list (column-name i result-set)) (l (+ i 1) m))))))

(define get-column-types
  (lambda (result-set)
    (let l ((i 1) (m (column-count result-set)))
 (if (= i m)
     (list (column-type i result-set))
     (append (list (column-type i result-set)) (l (+ i 1) m))))))

(define backup-table
  (lambda (table-name file-name)
    (call-with-output-file file-name
      (lambda (f)
 (let ( (rs (q (string-append "select * from " table-name))))
   (write (get-column-names rs) f)
   (write (get-column-types rs) f)
   (let l ()
     (if (->boolean (next rs))
  (begin
  (write (read-row rs) f)
  (l)
  )
  ())
     )
     )))))

;(set-max-rows statement (->jint 1000))


;(n)
;(desc)

;; Fetch PTS+WOs touples

;; Iterate touples
;; if doesn't find Req+Wo in scheduling_request
;;    discard record
;; else
;;    fetch last WO status
;;    determine trans_status_cde using release_status_cde
;;    if it is pending
;;       generate insert



;(q "select Wo_req_num,  wo_wo_number,  wo_status,
;       wo_est_complete_date,  wo_completion_date,
;       wo_release_est_date,  wo_requested_scheduling_dte,
;       sched_trans_status,  wo_scheduling_trans_type_cde
;       from dbo.workord order by Wo_req_num, wo_wo_number
;       ")

(define read-row
  (lambda (result-set)
    (let l ((i 1) (m (column-count result-set)))
      (if (not (> i m))
   (append (list `(,(string->symbol (column-name i result-set)) ,(d i
result-set))) (l (+ i 1) m))
         ()
         ))))

(define counter 0)
(define reset-counter
  (lambda ()
    (set! counter 0)))
(reset-counter)
(define display-and-update-counter
  (lambda ()
    (begin
      (set! counter (+ counter 1))
      (when (= 0 (modulo counter 100)) (display "\r") (display counter))
      )))

(define load-result-set-data
  (lambda (result-set)
    (let l ()
      (if (->boolean (next result-set))
          (begin
            (display-and-update-counter)
            (let ((r (read-row result-set)))
              (append (list r) (l))
              )
            )
          ()))))

(define write-result-set-data
  (lambda (result-set filename)
    (call-with-output-file filename
      (lambda (f)
 (display "(" f)
 (let l ()
   (if (->boolean (next result-set))
       (begin
  (display-and-update-counter)
  (write (read-row result-set) f)
  (l)
  )
       ()))
 (display ")" f)
))))

(define do-with-each-ptswo-combo
  (lambda (f c)
     (let l ((data c))
       (unless (eqv? data '())
        (let (
       (req (caar data))
       (wo  (cadar data))
       )
   (f req wo)
   (l (cdr data)))))))

(define c1-v 0)
(define c1 (lambda ()
      (set! c1-v (+ 1 c1-v))))
(define c2-v 0)
(define c2 (lambda ()
      (set! c2-v (+ 1 c2-v))))

(define get-from-alist
  (lambda (field-name hash)
    (begin
      (let ( (ret-value (assoc field-name hash)) )
    (begin
      (if (eqv? #f ret-value) "null"
          (cadr ret-value))
      )))))

(define sq
  (lambda (s)
    (string-append "'" s "'")))

; ----

(display "\n\nLoading old scheduling status history data.\n\n")
(define scheduling_status_history_old_data
  (load-result-set-data
   (qa-q
    (string-append "select request_id, release_status_cde,
approver_by_racf_id, action_date from "
     scheduling_status_history_table_old))))

(define m001
  (lambda (record)
    (begin
      (let (
     (req (cadr (assoc '|request_id|   record)))
     (wo  (cadr (assoc '|release_status_cde| record)))
     )
 (list (list req wo) record)
 )
      )))

(define tmp (map m001 scheduling_status_history_old_data))

(import hashtable)

(define old_scheduling_request_data_hash (alist->hashtable tmp))

; ---

;(define get-scheduling-status-history-data
;  (lambda (request_id release_status_cde)
;    (let ( (result-set (qa-q (string-append
;      "select approver_by_racf_id, action_date from "
;      scheduling_status_history_table_old
;      " where request_id = "
;      request_id
;      " and release_status_cde = "
;      (sq release_status_cde)
;      )
;     )))
;      (n result-set)
;      (read-row result-set))))

(define get-scheduling-status-history-data
  (lambda (request_id release_status_cde)
    (car (hashtable/get old_scheduling_request_data_hash (list request_id
release_status_cde)))))

(display "\n\nLoading priority information from the request table...\n\n")
(reset-counter)
(define req_priorities (load-result-set-data (prod-q "select Req_Num,
release_priority, group_priority, sub_group_priority from dbo.Request
                   where not (release_priority is null AND group_priority is
null AND sub_group_priority is null)")))
(define m002
  (lambda (record)
    (display-and-update-counter)
    (begin
      (let (
     (req (cadr (assoc '|Req_Num|   record)))
     )
 (list req record)
 )
      )))

(define tmp2 (map m002 req_priorities))

(define req_hash (alist->hashtable tmp2))

(define priority-get
  (lambda (field req_num)
    (let ( (priorities (hashtable/get req_hash req_num)) )
      (if (eqv? #f priorities)
   "null"
   (let ((field-value (cadr (assoc field (car priorities)))))
     (if (string=? (string-trim field-value) "")
  "null"
  field-value
  )
     )
   ))))

(define sql-log (open-output-file
"this-is-it_started_2007-03-01_23h42m.log"))

(define display-and-run
  (lambda (s)
    (let ((appended-string  (string-append "\n" s "\n") ))
      ;(display appended-string)
      (write appended-string sql-log)
      ;(flush-output-port sql-log)
      (execute-update (create-statement (prod-new-connection)) (->jstring
s))
    )))


(define toString
  (lambda (v)
    (let ( (o (open-output-string)) )
      (display v o)
      (get-output-string o))))

(define clean
  (lambda ()
    (display-and-run (string-append "DELETE FROM " scheduling_request_table
" WHERE submitted_date < '02-28-2007'"))
    (display-and-run (string-append "DELETE FROM "
scheduling_status_history_table " WHERE trans_status_cde is NULL"))))

(reset-counter)
(define go
  (lambda ()
    (display "\n\n ERASING old records from scheduling tables...\n\n")
    (clean)
    (display "\n\n MIGRATING DATA! \n\n")
    (let l ( (result-set
       (qa-q (string-append "SELECT request_id, trans_type_cde, req_num,
wo_num, release_type_cde, submitter_by_racf, submitted_date,
release_req_date, release_status_cde FROM " scheduling_request_table_old "
where release_status_cde is not null "))))
      (if (->boolean (next result-set))
   (begin
     (display-and-update-counter)

     (call/cc
      (lambda (skip)
        (let* ((r (read-row result-set))
        (release_status_cde (get-from-alist '|release_status_cde| r))

        (trans_status_cde
         (cond ((string=? release_status_cde "1") "D")
        ((string=? release_status_cde "2") "A")
        ((string=? release_status_cde "3") "P")
        ((string=? release_status_cde "4") "A")
        ((string=? release_status_cde "5") "C")
        (else
         (begin
    (display
     (string-append "\n\nERROR - IGNORING RECORD:\n\n---\n" (toString r) "
\n--- \n\n" ))(skip #f)))))
        )
   (let ( (ga (lambda (field)
       (get-from-alist field r))) )
     (if (string=? trans_status_cde "P")
         (begin

    (display-and-run
     (string-append
      "\n\n"
      "INSERT INTO  " scheduling_request_table
      " ( "
      " request_id, "
      " trans_type_cde, "
      " release_type_cde, "
      " submitter_by_racf, "
      " submitted_date, "
      " release_req_date, "
      " req_num, "
      " wo_num, "
      " sub_group_priority_rnk, "
      " group_priority_rnk, "
      " overall_priority_rnk) VALUES ( "

      (ga '|request_id|) ", "
      (sq (ga '|trans_type_cde|))  ", "
      (sq (ga '|release_type_cde|)) ", "
      (sq (ga '|submitter_by_racf|)) ", "
      (sq (ga '|submitted_date|)) ",  "
      (sq (ga '|release_req_date|))  ", "
      (sq (ga '|req_num|)) ", "
      (sq (ga '|wo_num|)) ", "
      (priority-get '|sub_group_priority| (ga '|req_num|) ) ", "
      (priority-get '|group_priority| (ga '|req_num|) ) ", "
      (priority-get '|release_priority| (ga '|req_num|) ) " ) "))

    )
         (begin
    (let (
          (scheduling_status_history_data
    (get-scheduling-status-history-data (get-from-alist '|request_id| r)
release_status_cde)
    )
          )


      (display-and-run
       (string-append
        "\n\n"
        "INSERT INTO  " scheduling_status_history_table
        " ( "
        " request_id, "
        " action_date, "
        " approver_by_racf_id, "
        " trans_type_cde, "
        " release_type_cde, "
        " submitted_by_racf_id, "
        " submitted_dte, "
        " release_requested_dte, "
        " req_num, "
        " wo_number, "
        " trans_status_cde, "
        " sub_group_priority_rnk, "
        " group_priority_rnk, "
        " overall_priority_rnk) VALUES ( "

        (ga '|request_id|) ", "
        (sq (get-from-alist '|action_date| scheduling_status_history_data) )
", "
        (sq (get-from-alist '|approver_by_racf_id|
scheduling_status_history_data) ) ", "

        (sq (ga '|trans_type_cde|))  ", "
        (sq (ga '|release_type_cde|)) ", "
        (sq (ga '|submitter_by_racf|)) ", "
        (sq (ga '|submitted_date|)) ",  "
        (sq (ga '|release_req_date|))  ", "
        (sq (ga '|req_num|)) ", "
        (sq (ga '|wo_num|)) ", "
        (sq trans_status_cde) ", "
        (priority-get '|sub_group_priority| (ga '|req_num|) ) ", "
        (priority-get '|group_priority| (ga '|req_num|) ) ", "
        (priority-get '|release_priority| (ga '|req_num|) ) " ) "))

      (when (string=? trans_status_cde "D")
     (display-and-run
      (string-append
       "UPDATE  dbo.request_rejection SET wo_num = "
       (sq (ga '|wo_num|))
       ", "
       "req_num = "
       (sq (ga '|req_num|)) " "
       " WHERE request_id = "
       (ga '|request_id|)
       ))

     )
      )
    )
         )
     )
   )))
     (l result-set))
   ()))))

(go)
