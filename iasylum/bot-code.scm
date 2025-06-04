(define *DEFAULT-OUTPUT-PREFIX* " : ")

(define *CONVERSATIONS-LIST-CACHE* (make-hashtable string=?))

(define (slack/clear-conversations-list-cache) (hashtable/clear! *CONVERSATIONS-LIST-CACHE*))

(define (slack/retrieve-full-conversation-list token)
  (define hashtable-entry-parameter (make-parameter*))
  (define my-result-blocking-parameter (make-blocking-parameter*))
  (define token-sha256 (sha256 token))

  (mutex/synchronize (mutex-of *CONVERSATIONS-LIST-CACHE*)
   (lambda ()
     (hashtable-entry-parameter (hashtable/get *CONVERSATIONS-LIST-CACHE* token-sha256))
     (unless (hashtable-entry-parameter) (hashtable/put! *CONVERSATIONS-LIST-CACHE* token-sha256 my-result-blocking-parameter))))
  (or
   (let ((hashtable-entry (hashtable-entry-parameter)))
     (and hashtable-entry
          (begin
            (log-info "Will wait for conversation for token with sha256 " token-sha256 "to be available...")
            (hashtable-entry))))
   (begin
     (log-info "Will retrieve the conversation list for token with sha256 " token-sha256 "...")
     (let ((complete-retrieved-list
            (let loop ((cursor ""))
              (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.conversations :as conversations]) (require '[clojure.tools.logging :as log])")))
              (let* ((token-var (random-var)) (cursor-var (random-var)) (result-var (random-var))
                     (nl
                      (try-with-exponential-backoff 'action-description: "Retrieving conversation list in slack." 'initial-interval-millis: 4096
                       'action:
                       (lambda ()
                         (clj
                          (string-append "
                          (let [ result-var (conversations/list {:api-url \"https://slack.com/api\" :token " token-var "}
                                              {:exclude_members \"true\" :cursor " cursor-var " :limit \"800\"
                                                             :types \"public_channel,private_channel,mpim,im\"
                                              }) ]
                            (list (map (fn [channel] {(get channel :id) (get channel :name)}) (get result-var :channels))
                              (get (get result-var :response_metadata) :next_cursor)))")
                          `((,token-var ,(->jstring token))  (,cursor-var ,(->jstring cursor)))))))
                     (next-cursor (->string (let ((jstr (clj "(nth nl 1)" `((nl ,nl)))))
                                              (if (java-null? jstr)
                                                  (throw (make-error (string-append* "Unable to retrieve full conversation list in slack.")))
                                                  jstr))))
                     (the-fetched-clojure-list (clj "(nth nl 0)" `((nl ,nl)))))
                (clj "(into {} (concat the-fetched-clojure-list next-or-empty))"
                     `((the-fetched-clojure-list ,the-fetched-clojure-list)
                       (next-or-empty
                        ,(if (string=? next-cursor "") (clj "[]")
                             (begin
                               (sleep-seconds 1)
                               (loop next-cursor))))))))))
       (my-result-blocking-parameter complete-retrieved-list)
       complete-retrieved-list))))

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
                                                      (when (not (nil? (-> e (.get :files))))
                                                       (when (= 1 (-> e (.get :files) (.count)))
                                                        (when (not (nil? (-> e (.get :files) (.get 0))))
                                                         (when (not (nil? (-> e (.get :files) (.get 0) (.get :url_private_download))))
                                                          (doto (get ~a (get ~a (get e :channel)))
                                                            (.put (new sisc.data.ImmutableString
                                                                 (str \"<\" (get e :user \"\") \"> \" (get e :text \"\")
                                                                      \" | PRIVATE-SLACK-FILE-URL: \" (-> e (.get :files) (.get 0) (.get :url_private_download)) ))))))))
                                                      (doto (get ~a (get ~a (get e :channel)))
                                                            (.put (new sisc.data.ImmutableString
                                                                 (str \"<\" (get e :user \"\") \"> \" (get e :text \"\")))))
                                                )
                                          )

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

(define* (slack/fetch-user-info (user-id: user-id) (token: token) (timeout-milliseconds: timeout-milliseconds #f))
  (assert user-id token (string? user-id) (string? token))
  (apply
   try-with-exponential-backoff
   `(
   action-description: "Attempt to retrieve information for slack user..."
   initial-interval-millis: 2000 max-interval-millis: 5000 add-jitter: #t
   log-error: ,log-error
   ,@(if timeout-milliseconds `(max-elapsed-time-millis: ,timeout-milliseconds) '())
   action:
   ,(lambda ()
     (let ((clj-result
            (mutex/synchronize (mutex-of clj)
                               (lambda ()
                                 (clj "(require 'clj-slack.users)")
                                 (clj "(clj-slack.users/info
                                  {:api-url \"https://slack.com/api\" :token strtoken}
                                  struserid)"
                                      `((strtoken ,(->jstring token))
                                        (struserid ,(->jstring user-id))))))))
           (deep-map (lambda (o) (if (clj-keyword? o) (clj-keyword->string o) o)) (jmap->alist clj-result)))))))

(define (slack-user-info/fetch-email user-info)
  (with/fc (lambda p #f)
           (lambda ()
             (->  user-info ((cute assoc ":user" <>)) cdr ((cute assoc ":profile" <>)) cadr ((cute assoc ":email" <>)) cadr))))

(define-nongenerative-struct attributed-message iasylum/bot/attributed-message (sender message))
(define slack/work-queue-bot
  (lambda* ((in-work-queue: in-work-queue #f)
            (out-work-queue: out-work-queue #f)
            (fetch-attributed-messages: fetch-attributed-messages #f)
            (use-web-api: use-web-api #f)
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
             (let ((the-inner-queue
                    (if (not fetch-attributed-messages)
                        (out-work-queue 'inner-queue)
                        (let ((email-retrieving-queue (make-queue)))
                          (start-worker (lambda (p)
                                          (let* ((expression '(: bos "<" (=> userid (*? any )) "> " ))
                                                 (message-text (match (irregex-split expression p) [(t) t] [() ""]))
                                                 (message-author (irregex-match-substring (irregex-search expression p) 'userid))
                                                 (has-author (and message-author (not (string=? message-author ""))))
                                                 (result (make-attributed-message (and has-author message-author)
                                                                                  message-text)))
                                            (out-work-queue 'put result)))
                                        email-retrieving-queue 'thread-name: "slack/email-retrieving-worker")
                          (email-retrieving-queue 'inner-queue)))))
               (if use-web-api
                   (slack-web-api/create-reader-bot
                    'name: name
                    'token: token
                    'fetch-bots-messages: #t
                    'channels: (list (cons channel the-inner-queue)))
                   (slack/create-reader-bot
                    'name: name
                    'token: token
                    'fetch-bots-messages: #t
                    'channels:  (list (cons channel the-inner-queue))))))
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
          (inq: inq) (outq: outq) (token: token #f)
          (command-prefix-list: command-prefix-list)
          (output-prefix: output-prefix *DEFAULT-OUTPUT-PREFIX*))
  (define bot-on-channel-display-fn
    (lambda params
      (inq 'put (irregex-replace/all
                 "\n"
                 (apply string-append*
                        (map display-string params))
                 (string-append* "\n" output-prefix)))))
    (define bot-on-channel-display-fn/clean
      (lambda params
        (inq 'put (apply string-append*
                         (map display-string params)))))
  (letrec
      ((resulting-fn
         (match-lambda*
          [('d . params) (apply bot-on-channel-display-fn params)]
          [('d/n . params) (apply bot-on-channel-display-fn (flatten (list "\n" params "\n")))]
          [('d/clean . params) (apply bot-on-channel-display-fn/clean params)]
          [('read-attributed-line-struct)
                        (let loop ()
                          (let ((orig (outq 'take)))
                            (let* ((v (attributed-message-message orig))
                                   (sender (attributed-message-sender orig)))
                              (let ((m (irregex-match `(seq bos (or ,@command-prefix-list)
                                                            (* (or whitespace ":"))
                                                            (submatch (* any))) v)))
                                (if m
                                    (make-attributed-message sender (string-trim-both (irregex-match-substring m 1)))
                                    (loop))))))]
          [('read-email-attributed-line-struct)
           (assert token)
           (match (resulting-fn 'read-attributed-line)
                  [(sender . message)
                   (make-attributed-message
                    (and sender
                         (slack-user-info/fetch-email
                          (slack/fetch-user-info 'user-id: sender 'token: token
                                                 ;; 10 minutes. Avoids lock-up when/if something is badly broken.
                                                 'timeout-milliseconds: 600000)))
                    message)])]
          [('read-attributed-line)
           (let ((v (resulting-fn 'read-attributed-line-struct))) (cons (attributed-message-sender v)
                                                                        (attributed-message-message v)))]
          [('read-email-attributed-line)
           (let ((v (resulting-fn 'read-email-attributed-line-struct))) (cons (attributed-message-sender v)
                                                                              (attributed-message-message v)))]
          [('read-line) (match (resulting-fn 'read-attributed-line) [(sender . message) message])]
          [('outq) outq]
          [('inq) inq])))
    resulting-fn))

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
             (output-prefix: output-prefix *DEFAULT-OUTPUT-PREFIX*)
             (use-web-api: use-web-api #f))
     (let* ((inq (make-queue))
            (outq (make-queue)))
       (slack/work-queue-bot 'in-work-queue: inq
                       'out-work-queue: outq
                       'name: bot-name
                       'channel: channel-name
                       'fetch-attributed-messages: #t
                       'token: token
                       'use-web-api: use-web-api)
       (bot-on-channel-command-processor 'inq: inq
                                         'outq: outq
                                         'token: token
                                         'command-prefix-list: command-prefix-list
                                         'output-prefix: output-prefix))))

(define irc/create-bot-on-channel (lambda p (nyi)))

(define (split-spaces str)
 ;; Handles unicode nbsp chars as well as regular spaces.
  (irregex-split (irregex `(+ (or "\u00a0" " "))) str))

(define* (bot/add-global-commands (token: token #f) bot commands)
  (match-lambda*
   [('read-attributed-line-struct)
    (let* ((m-struct (bot 'read-attributed-line-struct))
           (what-was-read (attributed-message-message m-struct))
           (sender (attributed-message-sender m-struct)))
      (for-each (match-lambda ([command fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn bot params))))
                         (['id: id command fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn 'id: id bot params))))
                         (['id: id command fn ':attributed:]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn 'id: id 'sender: sender bot params))))
                         (['id: id command fn ':attributed-email:]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (let ((sender-email (and sender token
                                                         (slack-user-info/fetch-email
                                                          (slack/fetch-user-info 'user-id: sender 'token: token
                                                                                 'timeout-milliseconds: 600000)))))
                                  (fn 'id: id 'sender-email: sender-email bot params)))))
                         ([command ':no-params: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
                                (log-trace "bot:" bot "params:" params)
                                (fn bot))))
                         (['id: id command ':no-params: fn]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((params (cdr (split-spaces what-was-read))))
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
                         (['id: id command ':one-param: fn ':attributed:]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((param (string-drop  what-was-read (string-length command))))
                                (log-trace "bot:" bot "param:" param)
                                (fn 'id: id 'sender: sender bot param))))
                         (['id: id command ':one-param: fn ':attributed-email:]
                          (log-trace "command:" command "what-was-read:" what-was-read)
                          (if (string-prefix? command what-was-read)
                              (let ((param (string-drop  what-was-read (string-length command))))
                                (log-trace "bot:" bot "param:" param)
                                (let ((sender-email (and sender token
                                                         (slack-user-info/fetch-email
                                                          (slack/fetch-user-info 'user-id: sender 'token: token
                                                                                 'timeout-milliseconds: 600000)))))
                                  (fn 'id: id 'sender-email: sender-email bot param)))))
                          )
                commands)
      m-struct)]
   [(anything-else . params) (apply bot (cons anything-else params))]))

;;; WATCHDOG support.

(define bot/watch-dog-maximum-allowed-pulseless-period-seconds (make-parameter* 375))
(define bot/assuager-cycle-seconds (make-parameter* (* (bot/watch-dog-maximum-allowed-pulseless-period-seconds) 4/5)))

(define bot/watchdog-recent-pulse-happened-retriever (make-parameter*))
(define (bot/redefine-watchdog-recent-pulse-happened)
  (bot/watchdog-recent-pulse-happened-retriever (make-expiring-parameter* 'expiration-seconds: (bot/watch-dog-maximum-allowed-pulseless-period-seconds))))

(define (bot/watchdog-assuage)
  (log-trace "bot/watchdog-assuage called.")
  ((bot/watchdog-recent-pulse-happened-retriever) #t))

(define* (bot/add-global-help-and-exit-commands bot help-text (exit-thunk: exit-thunk (lambda () (j "System.exit(0);"))))
  (let ((new-bot
         (bot/add-global-commands bot `(("/help" ,(lambda (bot params)
                                                (bot 'd (string-append*
                                                         "\n```"
                                                         (add-between "\n" help-text "```")))))
                                        ("/bot-pulse" ,(lambda (bot params)
                                                     (bot/watchdog-assuage)
                                                     (bot 'd (string-append* "Up @ "
                                                                             (iso-8601-timestamp)))))
                                        ("/exit" ,(lambda (bot params)
                                                    (bot 'd/n "Will exit in 3 seconds.")
                                                    (sleep-seconds 3)
                                                    (exit-thunk)))))))
    new-bot))

;; Terminal interaction->bot translation.
;;
;; This will find every file path inside a given directory and create an associated
;; command. Stdout and stderr from the command will be redirected to bot messages,
;; and stdin wil be fetched from bot 'read-line as well.
;; If the chosen directory is /example/ and there is a /example/nested/xpto file it will
;; become the /nested/xpto command for the bot. To see it in action call this with
;; /base/bedlam/iasylum/scripts-directory-example and /demo and /nested/works_too
;; commands will be created.
(define* (bot/add-scripts-directory-contents-as-commands (token: token #f)
                                                         bot directory)
  (define resulting-bot (make-parameter* bot))
  (let ((script-files (with-current-url (string-append directory "/") (lambda () (rglob "."))))) ;
    (for-each
     (lambda (script-file)
       (log-info "Adding slash command for script. " script-file)
       (and-let*
             ((command-str (irregex-replace '(seq bos "./") script-file "/"))
              (handler (lambda* ((id: id) (sender-email: email) bot param)
                                (and-let* ((json-param (scheme->json param))
                                           (my-sink (lambda (p) (bot 'd/n p)))
                                           (reader-thunk (lambda ()
                                                           (let ((read-line-result
                                                                  (string-append (bot 'read-line) "\n")))
                                                             read-line-result))))
                                  (r-base 'cmd-list: (list (string-append directory "/" script-file) id email json-param)
                                          (create-unary-function-based-output-port my-sink)  (mutex/new)
                                          (create-unary-function-based-output-port my-sink)  (mutex/new)
                                          (create-thunk-based-input-port reader-thunk) (mutex/new))
                                  ))))
         (resulting-bot (bot/add-global-commands 'token: token (resulting-bot)
                                                 (if token
                                                     `([id: ,(current-thread-name) ,command-str ,handler :attributed-email:])
                                                     `([id: ,(current-thread-name) ,command-str ,handler :attributed:]))))))
     script-files))
  (resulting-bot))

;; WATCHDOG

(define bot/suppress-watchdog? (make-parameter* (get-env "SUPPRESS_WATCHDOG")))

;; Monitor slack-based access health. Guarantees access to channels where this runs. Shutdown if any issues are found.
(define* (bot/enable-watchdog (failure-callback: failure-callback (lambda ignored #t))
                              (channel-name: channel-name)
                              (slack-token: slack-token)
                              (pulse-command: pulse-command))
  ;; Ensure expiring assuage matches bot/watch-dog-maximum-allowed-pulseless-period-seconds
  (bot/redefine-watchdog-recent-pulse-happened)

  ;; Avoids immediate triggering during startup.
  (bot/watchdog-assuage)

  (thread/spawn* 'thread-name: "watchdog"
                 (lambda () (let loop ()
                         (sleep-seconds 16)
                         (when (bot/suppress-watchdog?) (loop))
                         (if ((bot/watchdog-recent-pulse-happened-retriever))
                             (begin
                               (log-trace "watchdog checked. everything ok.")
                               (loop))
                             (begin
                               (thread/spawn (lambda ()
                                               (log-error "watchdog triggered. shutting down.")
                                               (failure-callback "watchdog triggered. shutting down.")))
                               (sleep-seconds 5)
                               (panic))))))

  (thread/spawn* 'thread-name: "bot/watchdog-assuager"
                 (lambda ()
                   (define pulse-bot-queue (make-queue))

                   (slack/work-queue-bot 'in-work-queue: pulse-bot-queue
                                         'name: "bot/watchdog-assuager"
                                         'channel: channel-name
                                         'token: slack-token)
                   (sleep-seconds 4)
                   (let loop ()
                        (pulse-bot-queue 'put pulse-command)
                        (sleep-seconds (bot/assuager-cycle-seconds))
                        (loop)))))

(define* (slack/retrieve-private-file-data (token: token) (max-size-bytes: max-size-bytes (expt 2 18)) (url: url) (allow-non-slack-urls: allow-non-slack-urls #f))
  (define expression '(: bos "https://files.slack.com/files-pri/" (*? any)))
  (if (irregex-search expression url)
      (http-call-get-headers/string url 'max-size-bytes: max-size-bytes 'headers: `(("Authorization" ,(string-append "Bearer " token))))
      (if allow-non-slack-urls
                (http-call-get-headers/string url 'max-size-bytes: max-size-bytes)
                (throw (make-error "To avoid leaking credentials slack/retrieve-private-file can only be used to retrieve URLs beginning with https://files.slack.com/files-pri/")))))

(define* (slack/upload-file (token: token) (filename: filename (pseudo-random-uuid)) (channels: channels) (contents-filename: contents-filename #f) (contents-string: contents-string #f))
  (define (process input-file)
    (assert (xor contents-filename contents-string))
    (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.files :as files]) (require '[clojure.tools.logging :as log])")))
    (let* ((token-var (random-var))
         (filename-var (random-var))
         (channels-var (random-var))
         (content-var (random-var))
         (final-result
          (clj
           (string-append "
              (let [ result-var (files/upload {:api-url \"https://slack.com/api\" :token " token-var "}
                                              " content-var "
                                              {:filename "filename-var " :filetype \"auto\" :channels " channels-var " } ) ]
                 (-> result-var (.get :file) (.get :url_private_download)))")
           `((,token-var ,(->jstring token))
             (,channels-var ,(->jstring (apply add-between (cons "," channels))))
             (,content-var ,(cond (contents-string
                                   (->jstring contents-string))
                                  (contents-filename
                                   (->jinput-stream input-file))
                                  (else (error "Bug."))))
             (,filename-var ,(->jstring filename))))))
      (->string final-result)))
  (if contents-filename
      (call-with-binary-input-file contents-filename process)
      (process #f)))

(define slack-web-api/create-reader-bot
  (lambda* ((name: name) (channels: channels) (token: token) (fetch-bots-messages: fetch-bots-messages))
    (let* (;; Retrieve the mapping from channel names to channel IDs.
           ;; This might need refreshing if channels change while the bot runs.
           (channel-id-map (alist-swap (jmap->alist (slack/retrieve-full-conversation-list token))))
           ;; Store the timestamp ('ts') of the last message processed for each channel ID.
           (last-timestamps (make-hashtable string=?))
           ;; Get the current time as the initial point for fetching messages.
           ;; Using exact-floor to get an integer timestamp string initially.
           (initial-timestamp (number->string (exact-floor (time-second (current-time time-utc))))))

      ;; Initialize the last timestamp for each channel we are monitoring.
      (for-each
       (lambda (channel-queue-pair)
         (let* ((channel-name (car channel-queue-pair))
                (channel-id (get channel-name channel-id-map)))
           (if channel-id
               (hashtable/put! last-timestamps channel-id initial-timestamp)
               (log-warn "Channel ID not found for name:" channel-name ". Cannot initialize timestamp."))))
       channels)

      ;; Start a separate polling thread for each channel.
      (for-each
       (lambda (channel-queue-pair)
         (let* ((channel-name (car channel-queue-pair))
                (output-queue-jobject (cdr channel-queue-pair)) ; Assuming this is the Java queue object
                (channel-id (get channel-name channel-id-map)))

           ;; If we couldn't find the channel ID, log an error and skip this channel.
           (unless channel-id
             (let ((error-msg (string-append* "Could not find channel ID for name:" channel-name ". Crashing.")))
               (log-error error-msg)
               (throw (make-error error-msg))))

           ;; Spawn a watched thread for polling this specific channel.
           (watched-thread/spawn
            'thread-name: (string-append "slack-web-api-poller-" channel-name)
            (lambda ()
              ;; The main polling loop for this channel.
              (let loop ()
                (sleep-seconds 6) ; Wait for 6 seconds before the next poll.

                ;; Retrieve the timestamp of the last message we processed for this channel.
                (let ((current-oldest (hashtable/get last-timestamps channel-id)))
                  (log-trace "Polling channel" channel-name "(" channel-id ") for messages since" current-oldest)

                  ;; Attempt to fetch conversation history using the Web API.
                  ;; Use exponential backoff to handle transient errors and rate limits.
                  (let* ((api-result
                          (try-with-exponential-backoff
                           'action-description: (string-append "Polling Slack channel history: " channel-name)
                           'initial-interval-millis: 1000
                           'max-interval-millis: 600000 ; Max wait 10 minutes
                           'max-elapsed-time-millis: 864000000 ; 10 days before giving up.
                           'log-error: log-error ; Log errors during backoff
                           'action:
                           (lambda ()
                             ;; Perform the HTTP POST request to the Slack API.
                             (log-trace "Perform the HTTP POST request to the Slack API.")
                             (let ((response-string
                                    (http-call-post-string/string
                                     "https://slack.com/api/conversations.history"
                                     ;; Encode parameters for the POST body.
                                     (alist-to-url-query-string
                                      `((token . ,token)
                                        (channel . ,channel-id)
                                        ;; Fetch messages strictly newer than the last processed one.
                                        (oldest . ,current-oldest)
                                        (limit . 100))) ; Fetch up to 100 messages per poll.
                                     ;; Set required headers.
                                     'headers: '(("Content-Type" . "application/x-www-form-urlencoded")))))
                               (log-trace "Response String: " response-string)
                               ;; Parse the JSON response if available.
                               (if response-string
                                   (json->scheme response-string)
                                   ;; Throw an error if the response is empty, triggering backoff.
                                   (throw (make-error "Failed to fetch conversation history, empty response.")))))))
                         ;; Keep track of the timestamp of the newest message processed in this batch.
                         (newest-processed-ts current-oldest))

                    ;; Process the API result.
                    (cond
                     ;; Handle permanent failure after backoff.
                     ((not api-result)
                      (log-error "Polling failed permanently for channel:" channel-name "after multiple retries.")
                      ;; Stop polling this channel.
                      (void))

                     ;; Handle specific API errors reported by Slack.
                     ((not (get "ok" api-result #f))
                      (log-error "Slack API error polling channel" channel-name "(" channel-id "):" (get "error" api-result "unknown_error"))
                      ;; Continue the loop; backoff already handled the delay.
                      (loop))

                     ;; Process successful API response.
                     (else
                      (let* ((messages (get "messages" api-result '#()))
                             ;(messages (if (null? me) '() (if (vector? me) (vector->list me) 
                             ;; API returns newest first; reverse to process oldest first.
                             (messages-oldest-first (reverse messages)))
                        (log-trace "Received" (length messages) "messages for channel" channel-name)

                        ;; Iterate through the fetched messages.
                        (for-each
                         (lambda (msg)
                           (let ((msg-ts (get "ts" msg)))
                             ;; Ensure the message timestamp is strictly greater than the last processed one.
                             ;; This prevents reprocessing the 'oldest' message itself.
                             (when (> (string->number msg-ts) (string->number current-oldest))
                               (let ((subtype (get "subtype" msg #f))
                                     (bot-id (get "bot_id" msg #f))
                                     (user-id (get "user" msg #f))
                                     (text (get "text" msg ""))
                                     (files (get "files" msg #f)))

                                 ;; Filter messages based on whether bot messages should be fetched.
                                 (when (or fetch-bots-messages (not bot-id))
                                   ;; Apply similar filtering logic as the RTM bot (ignore edits, threads, etc.)
                                   (when (and user-id text (not (get "edited" msg #f)) (not (get "thread_ts" msg #f)))
                                     ;; Extract file URL if present.
                                     (let* ((file-url-part
                                             (and files (> (vector-length files) 0)
                                                  (let ((file-info (vector-ref files 0)))
                                                    (get "url_private_download" file-info #f))))
                                            ;; Format the message string.
                                            (formatted-msg
                                             (string-append* "<" user-id "> " text
                                                             (if file-url-part (string-append* " | PRIVATE-SLACK-FILE-URL: " file-url-part) ""))))
                                       (log-trace "Queueing message from" channel-name ":" formatted-msg)
                                       ;; Put the formatted message into the output queue as an ImmutableString, matching the RTM bot's behavior.
                                       (j "queue.put(msg);" `((queue ,output-queue-jobject) (msg ,(java-wrap formatted-msg))))
                                       ;; Update the timestamp of the newest message processed in this batch.
                                       (set! newest-processed-ts msg-ts)
                                       )))))))
                         messages-oldest-first)

                        ;; If we processed any new messages, update the last timestamp for this channel.
                        (when (> (string->number newest-processed-ts) (string->number current-oldest))
                          (hashtable/put! last-timestamps channel-id newest-processed-ts))

                        ;; TODO: Implement pagination handling if necessary.
                        ;; Check api-result for response_metadata.next_cursor and loop API calls within this iteration if present.
                        ;; For simple polling of recent messages, this might not be needed if 100 messages/10s is sufficient.

                        ;; Continue the loop for the next poll.
                        (loop)))))))))))
       channels))
    ;; Implicitly return #t after setting up the threads, similar to the RTM version.
    #t))

(define (slack/get-text-or-url-link-text string)
 ;; This receives a string formatted as a link as explained on
 ;; https://api.slack.com/reference/surfaces/formatting#linking-urls and returns the link text,
 ;; or simply text which is returned as is.
  (irregex-match-substring (irregex-search  (irregex '(or (seq "<" (*? any) "|" (submatch-named to-be-extracted (: (*? any))) ">")
                                                          (seq "<" (submatch-named to-be-extracted (: (*? any))) ">")
                                                          (submatch-named to-be-extracted (seq (* any))))) string) 'to-be-extracted))

(define (slack/get-private-file-url string)
  (irregex-match-substring (irregex-search  (irregex '(seq (*? any) "| PRIVATE-SLACK-FILE-URL: " (submatch-named to-be-extracted (: (*? any))) eos))  string) 'to-be-extracted))
