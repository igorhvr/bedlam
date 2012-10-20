(define-syntax make-infobot
  (syntax-rules ()
    ((_ (crib help handler) ...)
        (infobot (make-plugin crib help 
                              (lambda args (apply handler args))) ...))))

(define listen-phrases
  '("Okay."
    "Sure."
    "I'm listening."
    "I'm all ears."))

(define quiet-phrases
  '("Fine, shutting up."
    "Consider my lips sealed."
    "Okay."
    "No problem."))

(define (be-quiet channel message left right)
  (if (and (equal? left "") (equal? right ""))
      (begin 
        (set-channel-bot-listening! channel #f)
        (random-elem quiet-phrases))
      #t))
      
(define (listen-up channel message left right)
  (if (and (equal? left "") (equal? right ""))
      (begin 
        (set-channel-bot-listening! channel #t)
        (random-elem listen-phrases))
      #t))

(define (make-standard-infobot)
  (make-infobot
    ; Plugins go here, in the form:
    ; (crib help handler)
   ("botsnack" "Feed me!" (lambda args ":)"))
   ("later tell " "\"tell <someone> <something>\" asks me to deliver a message to someone next time they speak."
    (tell 'later))   
   ("tell " "\"tell <someone> <something>\" asks me to deliver a message to someone as soon as I see them in a channel."
    (tell 'now))
   ("what is "
    "\"What is <something>\" asks for information about the something."
    (*-is? 'what))
   ("what are "
    "\"What are <somethings>\" asks for information about the somethings."
    (*-is? 'what))
   ("where is "
    "\"Where is <something>\" asks for the location of the something."
    (*-is? 'where))
   ("where are "
    "\"Where are <somethings>\" asks for the location of the somethings."
    (*-is? 'where))
   ("who is "
    "\"Who is <someone>\" asks for information about someone."
    (*-is? 'what))
   ("forget " "\"forget <something>\" asks me to forget what I know about something."
    forget)
   ("seen " "\"seen <someone>\" asks for the last time I saw someone speak."
    seen)
   ("be quiet" "Asks me to only speak when spoken to" be-quiet)
   ("shut up" "Same as above." be-quiet)
   ("log" "log <on|off> asks me to turn on/off logging for this channel."
    chanlog)
   ("listen up" "I'll start conversing to any message I see." listen-up)
   ("eval" "\"eval <s-expression>\" causes me to evaluate the given s-expression in R5RS Scheme and return the result"
    stateless-eval)
   ("pretty-print " "\"pretty-print <s-expression>\" causes me to format the given expression in nice way."
    pprint)
   ("expand " "\"expand <s-expression>\" causes me to expand all the macros in the s-expression, and pretty-print the vanilla Scheme form."
    expand)
   ("optimize " "\"optimize <s-expression>\" causes me to expand all the macros in the s-expression, and pretty-print the optimized vanilla Scheme form."
    do-optimize)
   ("express " "\"express <s-expression>\" will print the uExp version of an optimized, compiled s-expression."
    do-express)
   ("dictionary" "\"dictionary <word>\" tells me to look up a word in Webster's dictionary." (dict "web1913"))
   ("weather" "\"weather <city>\" asks me for the current weather in a city" get-weather)
   ("yow" "A bit of randomness from Zippy the Pinhead." yow)
   ("bart" "Bart Simpson chalkboard punishments" bart)
   ("join " "\"join <channel-name>\" asks me to join the channel as an infobot"
    join-chan)
   ("scheme-channel " "\"scheme-channel <channel-name>\" creates a scheme channel with the given name" make-schemechan)
;   ("part " "\"part <channel-name>\" asks me to leave a channel I'm in." request-part)
   ("pasted " "" (lambda args (random-elem paste-responses)))
   ("locate " "\"locate <someone>\" asks me to tell you where I see someone."
    locate)
   (" is also " "\"<fact> is also <something>\" tells me that the first term which I already know can also be named by the second." learn-aka)
   (" is aka " "\"<fact> is also <something>\" tells me that the first term which I already know can also be named by the second." learn-aka)
   (" is at " "\"<something> is at <somewhere>\" defines the location of a term for later recollection by \"where is\"."
    (learn 'where))
   (" is " "\"<something> is <something else>\" defines a term for later recollection by \"what is\"."
    (learn 'what))
   (#t "<something>? asks me what something is" default-plugin)
))

(define default-plugin 
  (let ([is-? (*-is? #f)])
   (lambda (channel message ignore term)
     (let ([rv (is-? channel message ignore term)])
       (if (string? rv)
           rv 
           (if (char=? (string-ref term 0) #\()
               (stateless-eval channel message "" term)
               #t))))))
                
(define paste-responses
 '("Wait, no I didn't.  Hey..."
   "Quit screwing around."
   "Thats really not funny, leave me out of it."
   "Wait, that wasn't me."))

(define (infobot . plugins)
  (lambda (channel message)
    (let-values ([(to-bot cleaned-message strict-tokens)
                  (full-parse (->string
                               (get-name (channel-bot channel)))
                              message)])
      (cond [(and (or (channel-bot-listens? channel) to-bot)
                  (equal? cleaned-message "help"))
             ; display help
             (send-messages
              (channel-bot channel) (message-nick message)
              (string-append
               (format "Hello, I'm ~a, a SISC Scheme Multibot.\n"
                            (->string (get-name (channel-bot channel))))
               "I respond to some natural language commands, such as:\n"
               "help - You're doing it.\n\n"))
             (for-each (lambda (p)
                         (send-messages bot
                                        (message-nick message)
                                        (string-append
                                         (plugin-crib p)
                                         " - "
                                         (plugin-help p))))
                       plugins)
             #f]
            [(or (channel-bot-listens? channel) to-bot)
             (let loop ([p plugins])
               (cond [(null? p) #t]
                     [(or (eqv? #t (plugin-crib (car p)))
                          (crib-match? (plugin-crib (car p))
                                   (message-text message)))
                      (let ([rv
                             (apply (plugin-handler (car p))
                                    channel message
                                    (crib-split (plugin-crib (car p))
                                                cleaned-message))])
                        (cond [(string? rv)
                               (begin (send-messages (channel-bot channel)
                                                     (message-source message)
                                                     rv)
                                      #f)]
                              [(eqv? rv 'continue)
                               (loop (cdr p))]
                              [else rv]))]
                     [else (loop (cdr p))]))]
            [else #f]))))
             