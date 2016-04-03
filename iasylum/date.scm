(require-extension (srfi 19)) ; date & time
(require-extension (srfi 1))

(module iasylum/date
  (add-days
   current-date-utc
   current-date-utc-less-one-hour
   get-day-index
   get-day-index-utc
   time->millis
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
                                                  (make-time time-duration 0 offset-seconds))))))
      (sha256+ (date-year-day date)
               (date-year date))))

  ;;
  ;; Return a string representing today.
  ;; It changes every day after 00:00 UTC.
  ;;
  (define (get-day-index-utc)
    (get-day-index (current-date 0)))

  ;;
  ;; It returns an exact number. Use (floor ...) to make it an integer.
  ;;
  (define (time->millis t)
    (+ (* 1000 (time-second t))
       (/ (time-nanosecond t) 1000000)))


  )
