(include "streams.scm")
(include "jdbc.scm")
(import s2j)
(import generic-procedures)
(import type-system)
(import hashtable)
(import threading)
(import string-io)
(import networking)
(import srfi-2)
(import srfi-9)
(import srfi-11)
(import srfi-19)
(import srfi-27)
(import jdbc)
(import streams)

(module metaphone
  (metaphone)
  (import srfi-1)
  (include "metaphone.scm"))
(import metaphone)
(include "pirc.scm")
(include "records.scm")
(include "irc.scm")
(include "channels.scm")
(include "parsing.scm")
(include "bots/anna.scm")
(include "plugins/info.scm")
(include "plugins/locate.scm")
(include "plugins/seen.scm")
(include "plugins/dict.scm")
(include "plugins/tell.scm")
(include "plugins/scheme.scm")
(include "plugins/zippy.scm")
(include "plugins/schemechan.scm")
(include "plugins/logging.scm")
(include "bots/infobot.scm")
(include "bots/logbot.scm")

(random-source-randomize! default-random-source)

(define bot-metaphone (metaphone bot-name))

(define (random-elem ls)
  (list-ref ls (random-integer (length ls))))

(define (make-standard-channel name bot)
  (make-channel-record name bot #f '()
                       (list logbot unawaybot seenbot (make-standard-infobot) annabot)
                       #f #f))

(define (dbconnect host name user password)
  (jdbc/connect (format "jdbc:postgresql://~a/~a?user=~a&password=~a" host name user password)))

(define private-channel)
(define bot)

(define (join-chan channel messsage ignore term)
;  (do-join term (make-standard-channel term (channel-bot channel)))
  "Okay.")

(define (onDisconnect)
  (let loop ()
    (unless (do-connect bot ircserver)
      (sleep 60)
      (loop)))
  (for-each (lambda (channel)
              (display (format "Joining ~a...~%" channel))
              (do-join channel (make-standard-channel channel bot)))
            channels)
  (for-each (lambda (channel)
              (display (format "Joining ~a...~%" channel))
              (make-schemechan private-channel #f #f channel))
            scheme-channels))

(define (connect-sarah)
  (set! dbcon (dbconnect dbhost dbname dbuser dbpasswd))
  (set! bot (make-bot bot-name))
  (set! private-channel (make-standard-channel "#private" bot))
  (init-tell)
  (onDisconnect))

;; Initializations
(init-zippy)
(init-schemechan-plugin)

; Add channel management hooks
(add-join-hook
 (lambda (channel sender login hostname)
   (add-user-to-channel channel sender)))

(add-part-hook
 (lambda (channel sender login hostname)
   (remove-user-from-channel
    channel (metaphone sender))))

(define dbcon)
(connect-sarah)