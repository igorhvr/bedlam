(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/random))
(require-extension (lib iasylum/clojure))

(module iasylum/bot
  (irc/work-queue-bot irc/create-bot-on-channel
   slack/work-queue-bot slack/create-bot-on-channel slack/create-reader-bot slack/clear-conversations-list-cache slack/fetch-user-info slack-user-info/fetch-email
   slack/retrieve-private-file-data slack/upload-file slack/get-text-or-url-link-text slack/get-private-file-url
   attributed-message? attributed-message-message attributed-message-sender
   bot/add-global-help-and-exit-commands bot/add-scripts-directory-contents-as-commands
   bot/add-global-commands  bot-on-channel-command-processor
   bot/watch-dog-maximum-allowed-pulseless-period-seconds bot/assuager-cycle-seconds bot/watchdog-recent-pulse-happened-retriever bot/watchdog-assuage bot/suppress-watchdog? bot/enable-watchdog
   )

  (include "bot-code.scm")
)

;; (define riad1 (make-queue)) (define riad2 (make-queue))
;; (reader-bot 'name: "kindle"
;;             'token: ""
;;      	 'channels: (list (cons "wheel-riadvargas1" (riad1 'inner-queue))
;;			          (cons "wheel-riadvargas2" (riad2 'inner-queue))))

;; Sample code:
;;(define inq (make-queue))(define outq (make-queue))(define mybot (work-queue-bot 'in-work-queue: inq 'out-work-queue: outq 'name: "igorhvr" 'channel: "smoke" 'token: "xoxp-1111111111-111111111111-111111111111-11111111111111111111111111111111"))
