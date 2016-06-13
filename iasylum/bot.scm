(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/work-queue))

(module iasylum/bot
  (work-queue-bot)
  
  (define work-queue-bot
    (lambda* ((in-work-queue: in-work-queue) (out-work-queue: out-work-queue) (name: name) (server-hostname: server-hostname) (server-port: server-port) (server-password: server-password) (channel: channel) )
             (let* ((inner-queue-out-work-queue (out-work-queue 'inner-queue))
                    (bot
                     (j "new org.pircbotx.PircBotX(new org.pircbotx.Configuration.Builder().setName(pmtname).addListener(new org.pircbotx.hooks.ListenerAdapter(){public void onGenericMessage(org.pircbotx.hooks.types.GenericMessageEvent event) {pmtoutqueue.put(new sisc.data.ImmutableString(event.getMessage()));}}).setServerHostname(pmtserverhostname).setServerPort(pmtserverport).setServerPassword(pmtserverpassword).buildConfiguration());"
                        `((pmtname ,(->jstring name))
                          (pmtoutqueue ,inner-queue-out-work-queue)
                          (pmtserverhostname ,(->jstring server-hostname))
                          (pmtserverport ,(->jint server-port))
                          (pmtserverpassword ,(->jstring server-password))))))
               (watched-thread/spawn (lambda() (j "pmtbot.startBot();"
                                                  `((pmtbot ,bot)))))
               
               (watched-thread/spawn (lambda() (let loop ()
                                                 (let ((nmsg (string-append "PRIVMSG #" channel " :" (in-work-queue 'take))))
                                                   (j "pmtbot.sendRawLineToServer(ln);"
                                                      `((pmtbot ,bot)
                                                        (ln ,(->jstring nmsg))))
                                                   (loop)))))
               bot)))

  ;; Sample code:
  ;;(define inq (make-queue))(define outq (make-queue))(define mybot (work-queue-bot 'in-work-queue: inq 'out-work-queue: outq 'name: "igorhvr" 'server-hostname: "localhost-ia" 'server-port: 16667 'server-password: "igorhvr/ia:igorhvr" 'channel: "ia"))

)
