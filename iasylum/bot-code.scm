(define work-queue-bot
  (lambda* ((in-work-queue: in-work-queue) (name: name) (channel: channel) (token: token #f))
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
	   #t))

(define create-reader-bot
    (lambda* ((name: name) (channels: channels) (token: token #f))
             (let* ((channelsvar (random-var))
		    (tokenvar (random-var)))
               (watched-thread/spawn (lambda ()
				       (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.groups :as groups]) (require '[clojure.tools.logging :as log])")))
				       (clj
					(format
                                         "(require '[slack-rtm.core :as rtm])
                                          (require '[clj-slack.groups :as groups])
                                          (require '[clojure.tools.logging :as log])
                                          (def conn (atom nil))

                                          (def channels-id (into {} (map
                                                                     (fn [group]
                                                                         {(get group :id) (get group :name)})
                                                                         (get (groups/list {:api-url \"https://slack.com/api\" :token ~a}) :groups))))

                                          (defn message-receiver [e]
                                                (when (and (contains? ~a (get channels-id (get e :channel)))
                                                           (not (get e :subtype false))
                                                           (not (get e :edited false))
                                                           (not (get e :thread_ts false)))
                                                      (doto (get ~a (get channels-id (get e :channel))) (.put (new sisc.data.ImmutableString (get e :text))))))

                                          (defn connect-to-slack [conn token]
                                                (reset! conn (rtm/rtm-connect token
                                                                              :on-close (fn [{:keys [status reason]}]
                                                                                            (log/error reason \"Previous connection with Slack was closed, retrying in 3 seconds...\")
                                                                                            (Thread/sleep 3000) ;; Waiting 3s before trying again
                                                                                            (connect-to-slack conn token))))
                                                (rtm/sub-to-event (:events-publication @conn) :message message-receiver))
                                          (connect-to-slack conn ~a)"
					 tokenvar channelsvar channelsvar tokenvar)
					`((,tokenvar ,(->jstring token))
					  (,channelsvar ,(->jobject channels))))))
               #t)))
