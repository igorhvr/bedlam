(define *DEFAULT-OUTPUT-PREFIX* " : ")

(define (slack/retrieve-full-conversation-list token)
  (let loop ((cursor ""))
    (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.conversations :as conversations]) (require '[clojure.tools.logging :as log])")))
    (let* ((token-var (random-var)) (cursor-var (random-var)) (result-var (random-var))
           (nl
            (clj
             (string-append "
              (let [ result-var (conversations/list {:api-url \"https://slack.com/api\" :token " token-var "}
                                  {:exclude_members \"true\" :cursor " cursor-var " :limit \"300\"
                                                 :types \"public_channel,private_channel,mpim,im\"
                                  }) ]
                (list (map (fn [channel] {(get channel :id) (get channel :name)}) (get result-var :channels))
                  (get (get result-var :response_metadata) :next_cursor)))")
             `((,token-var ,(->jstring token))
               (,cursor-var ,(->jstring cursor)))))
           (next-cursor (->string (let ((jstr (clj "(nth nl 1)" `((nl ,nl)))))
                         (if (java-null? jstr)
                             (throw (make-error (string-append* "Unable to retrieve full conversation list in slack.")))
                             jstr))))
           (the-fetched-clojure-list (clj "(nth nl 0)" `((nl ,nl)))))
      (clj "(into {} (concat the-fetched-clojure-list next-or-empty))"
           `((the-fetched-clojure-list ,the-fetched-clojure-list)
             (next-or-empty
              ,(if (string=? next-cursor "") (clj "[]")
                   (loop next-cursor))))))))

(define slack/create-reader-bot
    (lambda* ((name: name) (channels: channels) (token: token) (fetch-bots-messages: fetch-bots-messages))
             (let* ((channelsvar (random-var))
		    (tokenvar (random-var))
                    (connect-to-slack-var (random-var))
                    (channels-id-var (random-var))
                    (message-receiver-var (random-var))
                    (conn-var (random-var))
                    (channels-id (clj "(into {} (concat conversations))" `((conversations ,(slack/retrieve-full-conversation-list token))))))
               (watched-thread/spawn 'thread-name: "slack/create-reader-bot" (lambda ()
				       (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.channels :as channels]) (require '[clj-slack.groups :as groups]) (require '[clojure.tools.logging :as log])")))
				       (clj
					(format "
                                          (def ~a (atom nil))

                                          (defn ~a [e]
                                                (when (and (contains? ~a (get ~a (get e :channel)))
                                                           ~a
                                                           (not (get e :edited false))
                                                           (get e :text false)
                                                           (not (get e :thread_ts false)))
                                                      (doto (get ~a (get ~a (get e :channel))) (.put (new sisc.data.ImmutableString (get e :text \"\"))))))

                                          (defn ~a [conn token]
                                                (reset! ~a (rtm/rtm-connect token
                                                                              :on-close (fn [{:keys [status reason]}]
                                                                                            (log/error reason \"Previous connection with Slack was closed, retrying in 3 seconds...\")
                                                                                            (Thread/sleep 3000) ;; Waiting 3s before trying again.
                                                                                            (~a conn token))))
                                                (rtm/sub-to-event (:events-publication @conn) :message ~a))
                                          (~a ~a ~a)"
					 conn-var
                                         message-receiver-var
                                         channelsvar
                                         channels-id-var
                                         (if (not fetch-bots-messages) "(not (get e :subtype false))" " ")
                                         channelsvar channels-id-var
                                         connect-to-slack-var
                                         conn-var
                                         connect-to-slack-var
                                         message-receiver-var
                                         connect-to-slack-var
                                         conn-var
                                         tokenvar)
					`((,tokenvar ,(->jstring token))
					  (,channelsvar ,(->jobject channels))
                                          (,channels-id-var ,channels-id))))))))

(define slack/work-queue-bot
  (lambda* ((in-work-queue: in-work-queue #f)
            (out-work-queue: out-work-queue #f)
            (name: name) (channel: channel) (token: token))
           ;; Nothing to do if we don't have at least one of those set.
           (assert (or in-work-queue out-work-queue))
           (when in-work-queue
             (watched-thread/spawn 'thread-name: "slack/message-posting-thread" (lambda()
                                     (mutex/synchronize (mutex-of clj) (lambda () (clj "(require 'clj-slack.chat)")))
                                     (let loop ()
                                       (let* ((msg (in-work-queue 'take)))
                                         ;; This allows us to recover from serious but possibly temporary conditions.
                                         ;; Real-life example:
                                         ;;  java.net.UnknownHostException: slack.com: Temporary failure in name resolution
                                         (try-with-exponential-backoff
                                          'action-description: "Attempt to post message to Slack..."
                                          'initial-interval-millis: 2000
                                          'max-interval-millis: 16000
                                          'add-jitter: #t
                                          'log-error: log-error
                                          'action:
                                                   (lambda ()
                                                     (mutex/synchronize (mutex-of clj)
                                                            (lambda ()
                                                              (clj "(require 'clj-slack.chat)
                                                                                  (clj-slack.chat/post-message
                                                                                  {:api-url \"https://slack.com/api\" :token strtoken}
                                                                                  strchannel  strmsg {:username strusername})"
                                                                   `((strchannel ,(->jstring channel))
                                                                     (strtoken ,(->jstring token))
                                                                     (strusername ,(->jstring name))
                                                                     (strmsg ,(->jstring msg))
                                                                     )
                                                                   )
                                                              ;; https://api.slack.com/docs/rate-limits#rtm
                                                              (sleep-milliseconds 1100)))))
                                         (loop))))))
           (when out-work-queue
             (slack/create-reader-bot
              'name: name
              'token: token
              'fetch-bots-messages: #t
              'channels:  (list (cons channel (out-work-queue 'inner-queue)))))
	   #t))

  (define irc/work-queue-bot
    (lambda* ((in-work-queue: in-work-queue) (out-work-queue: out-work-queue) (name: name) (server-hostname: server-hostname) (server-port: server-port) (server-password: server-password) (channel: channel) )
             (let* ((inner-queue-out-work-queue (out-work-queue 'inner-queue))
                    (pmtoutqueuevarname (random-var))
                    (channelvar (random-var))
                    (pmtnamevar (random-var))
                    (botvar (random-var))
                    (bot
                     (j (format "new org.pircbotx.PircBotX(new org.pircbotx.Configuration.Builder().setName(~a).setAutoReconnect(true).	setAutoReconnectAttempts(999999).setAutoReconnectDelay(5000).setSocketFactory(javax.net.ssl.SSLSocketFactory.getDefault()).setCapEnabled(true).addCapHandler(new org.pircbotx.cap.TLSCapHandler(new org.pircbotx.UtilSSLSocketFactory().trustAllCertificates(), true)).addListener(new org.pircbotx.hooks.ListenerAdapter(){
    public void onMessage(org.pircbotx.hooks.events.MessageEvent event) {
        if (~a.equals(event.getChannel().getName())) {
           ~a.put(new sisc.data.ImmutableString(event.getMessage()));
        }
      }
    }).setServerHostname(pmtserverhostname).setServerPort(pmtserverport).setServerPassword(pmtserverpassword).buildConfiguration());"
                                pmtnamevar channelvar pmtoutqueuevarname)
                        `((,pmtnamevar ,(->jstring name))
                          (,pmtoutqueuevarname ,inner-queue-out-work-queue)
                          (pmtserverhostname ,(->jstring server-hostname))
                          (pmtserverport ,(->jint server-port))
                          (,channelvar ,(->jstring (string-append "#" channel)))
                          (pmtserverpassword ,(->jstring server-password))))))
               (watched-thread/spawn (lambda() (j (format "~a.startBot();" botvar)
                                                  `((,botvar ,bot)))))

               (watched-thread/spawn (lambda() (let loop ()
                                                 (let ((nmsg (string-append "PRIVMSG #" channel " :" (in-work-queue 'take))))
                                                   (j (format "~a.sendRawLineToServer(ln);" botvar)
                                                      `((,botvar ,bot)
                                                        (ln ,(->jstring nmsg))))
                                                   (sleep 550)
                                                   (loop)))))
               bot)))

  ;; Sample code:
  ;;(define inq (make-queue))(define outq (make-queue))(define mybot (irc/work-queue-bot 'in-work-queue: inq 'out-work-queue: outq 'name: "igorhvr" 'server-hostname: "localhost-ia" 'server-port: 16667 'server-password: "igorhvr/ia:igorhvr" 'channel: "ia"))

(define* (bot-on-channel-command-processor
          (inq: inq) (outq: outq) (command-prefix-list: command-prefix-list)
          (output-prefix: output-prefix *DEFAULT-OUTPUT-PREFIX*))
  (define bot-on-channel-display-fn
    (lambda params
      (inq 'put (irregex-replace/all
                 "\n"
                 (apply string-append*
                        (map display-string params))
                 (string-append* "\n" output-prefix)))))
         (match-lambda*
          [('d . params) (apply bot-on-channel-display-fn params)]
          [('d/n . params) (apply bot-on-channel-display-fn (flatten (list "\n" params "\n")))]
          [('read-line) (let loop ()
                          (let ((v (outq 'take)))
                            (let ((m (irregex-match `(seq bos (or ,@command-prefix-list)
                                                          (* (or whitespace ":"))
                                                          (submatch (* any))) v)))
                              (if m
                                  (string-trim-both (irregex-match-substring m 1))
                                  (loop)))))]
          [('outq) outq]
          [('inq) inq]))

;;
;; Example of usage:
;;
;; (define bot
;;  (slack/create-bot-on-channel
;;   'channel-name: <SLACK-CHANNEL-NAME>
;;   'bot-name: <ANYTHING>
;;   'token: <SLACK-TOKEN>
;;   'command-prefix-list: '("c:" "C:" "command:")
;;   'output-prefix: ""))
;;
;; (bot 'd/n "hello world")
;; => "hello world" (in the slack channel)
;;
;; (bot 'read-line)
;; => "any string typed after "c:", "C:" or "command:" in the slack channel"
;; e.g. if you type "c: hello" in the channel, the result of this fn would be "hello".
;;
(define slack/create-bot-on-channel
   (lambda* ((channel-name: channel-name)
             (bot-name: bot-name)
             (token: token)
             (command-prefix-list: command-prefix-list)
             (output-prefix: output-prefix *DEFAULT-OUTPUT-PREFIX*))
     (let* ((inq (make-queue))
            (outq (make-queue)))
       (slack/work-queue-bot 'in-work-queue: inq
                       'out-work-queue: outq
                       'name: bot-name
                       'channel: channel-name
                       'token: token)
       (bot-on-channel-command-processor 'inq: inq
                                         'outq: outq
                                         'command-prefix-list: command-prefix-list
                                         'output-prefix: output-prefix))))

(define irc/create-bot-on-channel (lambda p (nyi)))

(define (bot/add-global-commands bot commands)
  (match-lambda*
   [('read-line)
    (let ((what-was-read (bot 'read-line)))
      (for-each (match-lambda ([command fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-string " " what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn bot params))))
                         (['id: id command fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-string " " what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn 'id: id bot params))))
                         ([command ':no-params: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-string " " what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn bot))))
                         (['id: id command ':no-params: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-string " " what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn 'id: id bot))))
                         ([command ':one-param: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((param (string-drop what-was-read (string-length command))))
                                (log-trace "bot:" bot "param:" param)
                                (fn bot param))))
                         (['id: id command ':one-param: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((param (string-drop  what-was-read (string-length command))))
                                (log-trace "bot:" bot "param:" param)
                                (fn 'id: id bot param))))
                          )
                commands)
      what-was-read)]
   [(anything-else . params) (apply bot (cons anything-else params))]))

(define* (bot/add-global-help-and-exit-commands bot help-text (exit-thunk: exit-thunk (lambda () (j "System.exit(0);"))))
  (let ((new-bot
         (bot/add-global-commands bot `(("/help" ,(lambda (bot params)
                                                (bot 'd (string-append*
                                                         "\n```"
                                                         (add-between "\n" help-text "```")))))
                                    ("/exit" ,(lambda (bot params)
                                                (bot 'd/n "Will exit in 3 seconds.")
                                                (sleep-seconds 3)
                                                (exit-thunk)))))))
    new-bot))
