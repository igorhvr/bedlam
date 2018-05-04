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
				       (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.groups :as groups])")))
				       (clj
					(format
					 "(require '[slack-rtm.core :as rtm])
                                          (require '[clj-slack.groups :as groups])

                          		  (def rtm-conn (rtm/rtm-connect ~a))

        		     		  (def events-publication (:events-publication rtm-conn))

					  (def channels-id (into {} (map
					  				  (fn [group]
					  				      {(get group :id) (get group :name)})
									  (get (groups/list {:api-url \"https://slack.com/api\" :token ~a}) :groups))))

              	               		  (rtm/sub-to-event events-publication :pong)
     		          		  (def dispatcher (:dispatcher rtm-conn))
		               		  (rtm/send-event dispatcher {:type \"ping\"}) ;; To avoid constant disconnects

		               		  (defn message-receiver [e]
                                                (when (and (contains? ~a (get channels-id (get e :channel)))
                                                           (not (get e :subtype false))
                                                           (not (get e :edited false))
                                                           (not (get e :thread_ts false)))
                                                      (doto (get ~a (get channels-id (get e :channel))) (.put (new sisc.data.ImmutableString (get e :text))))))
		               		  (rtm/sub-to-event events-publication :message message-receiver)"
					 tokenvar tokenvar channelsvar channelsvar)
					`((,tokenvar ,(->jstring token))
					  (,channelsvar ,(->jobject channels))))))
               #t)))
