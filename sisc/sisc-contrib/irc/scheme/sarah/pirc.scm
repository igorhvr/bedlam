(import s2j)

(define-generic-java-methods ban change-nick connect dcc-accept-chat-request
  dcc-receive-file dcc-send-chat-request dcc-send-file de-op de-voice
  disconnect get-finger get-login get-max-line-length
  get-message-delay get-name get-outgoing-queue-size get-password
  get-port get-server get-version ip-to-long is-connected join-channel
  kick (pirc:log log) long-to-ip op part-channel quit-server reconnect
  send-action send-ctcp-command send-invite send-message send-notice
  send-raw-line set-finger set-login set-message-delay set-mode
  set-name set-topic set-verbose set-version start-ident-server un-ban voice)

(define (call-handler procname . args)
  (let ((p (getprop procname)))
    (when p (apply p args))))

(define (error-handler m e f)
  (print-exception (make-exception m e)))

(define-java-class <sisc.contrib.pirc.pirc-interface>)
(define (make-bot name) (java-new <sisc.contrib.pirc.pirc-interface> (->jstring name)))

