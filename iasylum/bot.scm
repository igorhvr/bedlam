(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/random))

(module iasylum/bot
  (work-queue-bot)
  
  (define work-queue-bot
    (lambda* ((in-work-queue: in-work-queue) (out-work-queue: out-work-queue) (name: name) (server-hostname: server-hostname) (server-port: server-port) (server-password: server-password) (channel: channel) )
             (let* ((inner-queue-out-work-queue (out-work-queue 'inner-queue))
                    (pmtoutqueuevarname (random-var))
                    (bot                     
                     (j (string-append "new org.pircbotx.PircBotX(new org.pircbotx.Configuration.Builder().setName(pmtname).setSocketFactory(javax.net.ssl.SSLSocketFactory.getDefault()).setCapEnabled(true).addCapHandler(new org.pircbotx.cap.TLSCapHandler(new org.pircbotx.UtilSSLSocketFactory().trustAllCertificates(), true)).addListener(new org.pircbotx.hooks.ListenerAdapter(){public void onMessage(org.pircbotx.hooks.events.MessageEvent event) {if(mchannel.equals(event.getChannel().getName())) " pmtoutqueuevarname ".put(new sisc.data.ImmutableString(event.getMessage()));}}).setServerHostname(pmtserverhostname).setServerPort(pmtserverport).setServerPassword(pmtserverpassword).buildConfiguration());")
                        `((pmtname ,(->jstring name))
                          (,(string->symbol pmtoutqueuevarname) ,inner-queue-out-work-queue)
                          (pmtserverhostname ,(->jstring server-hostname))
                          (pmtserverport ,(->jint server-port))
                          (mchannel ,(->jstring (string-append "#" channel)))
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
