(define-record-type :plugin
  (make-plugin crib help handler)
  plugin?
  (crib plugin-crib)
  (help plugin-help)
  (handler plugin-handler))

(define-record-type :channel
  (make-channel-record name bot quiet members handlers seed logfile)
  channel-record?
  (name channel-name)
  (bot channel-bot)
  (ai channel-has-ai?)
  (quiet channel-bot-listens? set-channel-bot-listening!)
  (members channel-members set-channel-members!)
  (handlers channel-handlers set-channel-handlers!)
  (seed channel-seed set-channel-seed!)
  (logfile channel-log-file set-channel-log-file!))

(define-record-type :command
  (make-command-record name help handler)
  command-record?
  (name command-name)
  (help command-help)
  (handler command-handler set-command-handler!))

(define-record-type :irc-message
  (make-irc-message private source nick metaphone-nick login host message)
  irc-message?
  (private message-is-private?)
  (source message-source)
  (nick message-nick)
  (metaphone-nick message-metaphone-nick)
  (login message-login)
  (host message-host)
  (message message-text))