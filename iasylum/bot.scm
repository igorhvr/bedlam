(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/work-queue))
(require-extension (lib iasylum/random))
(require-extension (lib iasylum/clojure))

(module iasylum/bot
  (work-queue-bot)
  
  (define work-queue-bot
    (lambda* ((in-work-queue: in-work-queue) (out-work-queue: out-work-queue) (name: name) (server-hostname: server-hostname) (server-port: server-port) (server-password: server-password) (channel: channel) (token: token #f))
             (let* ((inner-queue-out-work-queue (out-work-queue 'inner-queue))
                    (pmtoutqueuevarname (random-var))
                    (fixedpmtoutqueuevarname (random-var))
                    (channelvar (random-var))
                    (fixedchannelvar (random-var))
                    (pmtnamevar (random-var))
                    (botvar (random-var))
                    (bot
                     (j (string-append fixedchannelvar "=" channelvar "; " fixedpmtoutqueuevarname "=" pmtoutqueuevarname "; " "new org.pircbotx.PircBotX(new org.pircbotx.Configuration.Builder().setName(" pmtnamevar ").setAutoReconnect(true).setAutoReconnectAttempts(9999).setAutoReconnectDelay(5000).setSocketFactory(javax.net.ssl.SSLSocketFactory.getDefault()).setCapEnabled(true).addCapHandler(new org.pircbotx.cap.TLSCapHandler(new org.pircbotx.UtilSSLSocketFactory().trustAllCertificates(), true)).addListener(new org.pircbotx.hooks.ListenerAdapter(){
    public void onMessage(org.pircbotx.hooks.events.MessageEvent event) {
        if (" fixedchannelvar ".equals(event.getChannel().getName())) {
           " fixedpmtoutqueuevarname ".put(new sisc.data.ImmutableString(event.getMessage()));
        }
      }
    }).setServerHostname(pmtserverhostname).setServerPort(pmtserverport).setServerPassword(pmtserverpassword).buildConfiguration());"
           )
                        `((,pmtnamevar ,(->jstring name))
                          (,pmtoutqueuevarname ,inner-queue-out-work-queue)
                          (pmtserverhostname ,(->jstring server-hostname))
                          (pmtserverport ,(->jint server-port))
                          (,channelvar ,(->jstring (string-append "#" channel)))
                          (pmtserverpassword ,(->jstring server-password))))))
               (watched-thread/spawn (lambda() (j (format "~a.startBot();" botvar)
                                                  `((,botvar ,bot)))))
               (watched-thread/spawn (lambda()
                                       (mutex/synchronize (mutex-of clj) (lambda () (clj "(require 'clj-slack.chat)")))
                                       (let loop ()
                                                 (let* ((msg (in-work-queue 'take))
                                                        (nmsg (string-append "PRIVMSG #" channel " :" msg)))

                                                   (if token
                                                       (mutex/synchronize (mutex-of clj)
                                                                          (lambda ()
                                                                            (clj "(require 'clj-slack.chat)
                                                                                  (clj-slack.chat/post-message
                                                                                  {:api-url \"https://slack.com/api\" :token strtoken}
                                                                                  strchannel  strmsg {:username strusername})"
                                                                                 `((strchannel ,(->jstring channel))

                                                                            ;; TODO: This should not be hard-coded. This token can be used
                                                                            ;; by an attacker. After a proper place is setup for This
                                                                            ;; the token should be discared/replaced using
                                                                            ;; https://api.slack.com/custom-integrations/legacy-tokens
                                                                                   (strtoken ,(->jstring token))
                                                          
                                                                                   (strusername ,(->jstring name))
                                                                                   (strmsg ,(->jstring msg))
                                                                                   )
                                                                                 )
                                                                            ))
                                                       (begin
                                                         (j (format "~a.sendRawLineToServer(ln);" botvar)
                                                            `((,botvar ,bot)
                                                              (ln ,(->jstring nmsg))))
                                                         ;; To avoid getting disconnected 
                                                         (sleep 2000)))

                                                   (loop)))))
               bot)))

  ;; Sample code:
  ;;(define inq (make-queue))(define outq (make-queue))(define mybot (work-queue-bot 'in-work-queue: inq 'out-work-queue: outq 'name: "igorhvr" 'server-hostname: "localhost-ia" 'server-port: 16667 'server-password: "igorhvr/ia:igorhvr" 'channel: "ia"))

)
