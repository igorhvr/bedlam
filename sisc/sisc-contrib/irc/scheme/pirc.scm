(import s2j)
(import generic-procedures)

(define-syntax define-generics 
  (syntax-rules ()
     ((_ c ...)
      (begin (define-generic c) ...))))  

(define-generics ban change-nick connect dcc-accept-chat-request
  dcc-receive-file dcc-send-chat-request dcc-send-file de-op de-voice
  disconnect get-finger get-login get-max-line-length
  get-message-delay get-name get-outgoing-queue-size get-password
  get-port get-server get-version ip-to-long is-connected join-channel
  kick log long-to-ip op part-channel quit-server reconnect
  send-action send-ctcp-command send-invite send-message send-notice
  send-raw-line set-finger set-login set-message-delay set-mode
  set-name set-topic set-verbose set-version start-ident-server un-ban voice)

(define (call-handler procname . args)
  (let ((p (getprop procname '*toplevel*)))
    (when p (apply p args))))

(define (make-bot name) (make (java-class "PircInterface") (->jstring name)))

