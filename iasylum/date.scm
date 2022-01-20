(require-extension (srfi 19)) ; date & time
(require-extension (srfi 1))

(module iasylum/date
  (add-days
   current-date-utc
   current-date-utc-less-one-hour
   get-day-index
   get-day-index-utc
   time->millis
   millis->time
   date->millis
   millis->date
   next-month
   month-before
   get-last-day-of-month
   milliseconds-duration->approximate-time-duration-human-string
   iso-8601-timestamp
   rfc3339-timestamp
   add-to-timestamp
   )

  (define (add-days date ndays)
    (let ((tdate (if (string? date) (string->date date "~Y-~m-~d") date)))
      (let ((date-result (time-utc->date (add-duration (date->time-utc tdate) (make-time 'time-duration 0 (* ndays 24 60 60))))))
        (if (string? date) (date->string date-result "~4") date-result))))

  (define (current-date-utc)
    (current-date 0))

  (define (current-date-utc-less-one-hour)
    (time-utc->date (subtract-duration (date->time-utc (current-date-utc)) (make-time 'time-duration 0 (* 60 60)))
                    0))

  ;;
  ;; Example of use: (get-day-index (current-date 0))
  ;; If you want to get the current day index in UTC, use (get-day-index-utc)
  ;;
  (define* (get-day-index date (offset-seconds: offset-seconds 0))
    (let ((date (if (= offset-seconds 0)
                    date
                    (time-utc->date (add-duration (date->time-utc date)
                                                  (make-time time-duration 0 offset-seconds)) 0))))
      (sha256+ (date-year-day date)
               (date-year date))))

  ;;
  ;; Return a string representing today.
  ;; It changes every day after 00:00 UTC.
  ;;
  (define (get-day-index-utc)
    (get-day-index (current-date 0)))

  (define (time->millis t)
    (floor (+ (* 1000 (time-second t))
              (/ (time-nanosecond t) 1000000))))

  (define (millis->time m)
    (let* ((exact-seconds (/ m 1000))
           (seconds (floor exact-seconds)))
      (make-time time-utc (floor (* (- exact-seconds seconds) 1000000000)) seconds)))

  (define (date->millis date)
    (time->millis (date->time-utc date)))

  (define (millis->date time-millis)
    (time-utc->date (millis->time time-millis)))

  (define (next-month month-year-pair)
    (and month-year-pair
         (let ((month-year-pair (cons (+ (car month-year-pair) 1)
                                      (cdr month-year-pair))))
           (if (> (car month-year-pair) 12)
               (cons 1 (+ (cdr month-year-pair) 1))
               month-year-pair))))

  (define (month-before month-year-pair)
    (and month-year-pair
         (let ((month-year-pair (cons (- (car month-year-pair) 1)
                                      (cdr month-year-pair))))
           (if (< (car month-year-pair) 1)
               (cons 12 (- (cdr month-year-pair) 1))
               month-year-pair))))

  (define (get-last-day-of-month month year)
    (->scm-object (j "java.time.LocalDate.of(year, month, 1).with(java.time.temporal.TemporalAdjusters.lastDayOfMonth()).getDayOfMonth();"
                     `((year ,(->jint year)) (month ,(->jint month))))))

  (define (milliseconds-duration->approximate-time-duration-human-string n)
    (if (< n 60000)
        (string-append* n " milliseconds")
        (->string (j "mooopp = new org.ocpsoft.prettytime.PrettyTime(new java.util.Date(0)); mooopp.formatDuration(mooopp.approximateDuration(new java.util.Date(millip)));" `((millip ,(->jlong n)))))))

  (define* (iso-8601-timestamp (ts (current-date-utc))) (date->string ts "~4"))

  (define* (add-to-timestamp (srfi-19-date: dt #f) (seconds: seconds #f))
    (when (or (not dt) (not seconds)) (throw (make-error "Missing one or both of srfi-19-date or seconds parameter. Usage: (add-to-timestamp 'srfi-19-date: (current-date-utc) 'seconds: 400)")))
    (time-utc->date (add-duration (date->time-utc dt) (make-time 'time-duration 0 seconds))))

  (create-shortcuts (iso-8601-timestamp -> rfc3339-timestamp))
  )
