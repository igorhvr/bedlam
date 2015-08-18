(require-extension (srfi 19)) ; date & time
(require-extension (srfi 1))

(module iasylum/date
  (add-days)

  (define (add-days date ndays)
    (let ((tdate (if (string? date) (string->date date "~Y-~m-~d") date)))
      (let ((date-result (time-utc->date (add-duration (date->time-utc tdate) (make-time 'time-duration 0 (* ndays 24 60 60))))))
        (if (string? date) (date->string date-result "~4") date-result))))
  )
