(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/random))
(require-extension (lib iasylum/clojure))

(module iasylum/bot
  (irc/work-queue-bot irc/create-bot-on-channel
   slack/work-queue-bot slack/create-bot-on-channel
   slack/create-reader-bot
   attributed-message? attributed-message-message attributed-message-sender
   bot/add-global-help-and-exit-commands bot/add-global-commands  bot-on-channel-command-processor)

  (include "bot-code.scm")
)

;; (define riad1 (make-queue)) (define riad2 (make-queue))
;; (reader-bot 'name: "kindle"
;;             'token: ""
;;      	 'channels: (list (cons "wheel-riadvargas1" (riad1 'inner-queue))
;;			          (cons "wheel-riadvargas2" (riad2 'inner-queue))))

;; Sample code:
;;(define inq (make-queue))(define outq (make-queue))(define mybot (work-queue-bot 'in-work-queue: inq 'out-work-queue: outq 'name: "igorhvr" 'channel: "smoke" 'token: "xoxp-1111111111-111111111111-111111111111-11111111111111111111111111111111"))
