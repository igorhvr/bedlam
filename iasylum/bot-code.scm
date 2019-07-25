(define work-queue-bot
  (lambda* ((in-work-queue: in-work-queue)
            (out-work-queue: out-work-queue #f)
            (name: name) (channel: channel) (token: token))

	   (watched-thread/spawn (lambda()
				   (mutex/synchronize (mutex-of clj) (lambda () (clj "(require 'clj-slack.chat)")))
				   (let loop ()
				     (let* ((msg (in-work-queue 'take)))
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
                                                            ))
				       (loop)))))
           (when out-work-queue
             (create-reader-bot
              'name: name
              'token: token
              'channels:  (list (cons channel (out-work-queue 'inner-queue)))))
           
	   #t))

(define create-reader-bot
    (lambda* ((name: name) (channels: channels) (token: token))
             (let* ((channelsvar (random-var))
		    (tokenvar (random-var)))
               (watched-thread/spawn (lambda ()
				       (mutex/synchronize (mutex-of clj) (lambda () (clj "(require '[slack-rtm.core :as rtm]) (require '[clj-slack.channels :as channels]) (require '[clj-slack.groups :as groups]) (require '[clojure.tools.logging :as log])")))
				       (clj
					(format
                                         "(require '[slack-rtm.core :as rtm])
                                          (require '[clj-slack.channels :as channels])
                                          (require '[clj-slack.groups :as groups])
                                          (require '[clojure.tools.logging :as log])
                                          (def conn (atom nil))

                                          (def channels-id (into {}
                                                             (concat
                                                                    (map
                                                                     (fn [channel]
                                                                         {(get channel :id) (get channel :name)})
                                                                         (get (channels/list {:api-url \"https://slack.com/api\" :token ~a}) :channels))
                                                                    (map
                                                                     (fn [group]
                                                                         {(get group :id) (get group :name)})
                                                                         (get (groups/list {:api-url \"https://slack.com/api\" :token ~a}) :groups)))))

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
                                                                                            (Thread/sleep 3000) ;; Waiting 3s before trying again.
                                                                                            (connect-to-slack conn token))))
                                                (rtm/sub-to-event (:events-publication @conn) :message message-receiver))
                                          (connect-to-slack conn ~a)"
					 tokenvar tokenvar channelsvar channelsvar tokenvar)
					`((,tokenvar ,(->jstring token))
					  (,channelsvar ,(->jobject channels))))))
               #t)))


