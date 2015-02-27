(require-extension (lib iasylum/jcode))


(module iasylum/hornetq
  (hornetq-transport-configuration
   hornetq-session
   hornetq-queue
   hornetq-producer hornetq-consumer
   hornetq-send hornetq-receive
   hornetq-get-message-object hornetq-get-message-jobject hornetq-get-message-body-buffer-as-string hornetq-get-message-properties
   build-standard-localhost-session-lambda
   build-standard-queue-lambda
   build-producer-lambda
   build-consumer-lambda
   hornetq->work-queue
   )

  
  (define hornetq-transport-configuration
    (lambda* (host (port 5445))
      (j "map = new java.util.concurrent.ConcurrentHashMap();
          map.put(\"host\", myhost);
          map.put(\"port\", myport);
          new org.hornetq.api.core.TransportConfiguration(org.hornetq.core.remoting.impl.netty.NettyConnectorFactory.class.getName(), map);"
         `((myhost ,(->jstring host))
           (myport ,(->jint port))))))

  (define-java-class <org.hornetq.api.core.TransportConfiguration> |org.hornetq.api.core.TransportConfiguration|)
  
  (define hornetq-session
    (lambda* (transports (username: username "") (password: password ""))
        (j "cf = org.hornetq.api.jms.HornetQJMSClient.createConnectionFactoryWithHA(org.hornetq.api.jms.JMSFactoryType.CF, transportsarray);
            if(\"\".equals(tusername) && \"\".equals(tpassword)) {
                connection = cf.createConnection();
            } else {
                connection = cf.createConnection(tusername,tpassword);
            }
            connection.start();
            session = connection.createSession(false, javax.jms.Session.DUPS_OK_ACKNOWLEDGE);"
       
            `((transportsarray ,(->jarray transports <org.hornetq.api.core.TransportConfiguration>))
              (tusername ,(->jstring username))
              (tpassword ,(->jstring password))))))
  
  (define (hornetq-queue queue-name)
    (j "orderQueue = org.hornetq.api.jms.HornetQJMSClient.createQueue(queuename);" `((queuename ,(->jstring queue-name)))))
  
  (define (hornetq-producer session queue)
    (let ((session (if (procedure? session) (session) session))
          (queue (if (procedure? queue) (queue) queue)))
      (j "session.createProducer(queue);" `((session ,session) (queue ,queue)))))
  
  (define (hornetq-consumer session queue)
    (let ((session (if (procedure? session) (session) session))
          (queue (if (procedure? queue) (queue) queue)))
      (j "session.createConsumer(queue);" `((session ,session) (queue ,queue)))))
  
  (define hornetq-send
    (lambda* (session producer (object: object #f) (properties: properties '()))
             (let ((message
                    (cond (object
                           (j "r=session.createObjectMessage();
                            r.setObject(obj);
                            r;" `((session ,session) (obj ,(->jobject object)))))
                          (else
                           (j "r=session.createMessage();
                               r;" `((session ,session)))))))
               
               (pam properties
                    (match-lambda ((key . value)
                              (j "message.setObjectProperty(pname, pvalue);" `((message ,message) (pname ,(->jstring key)) (pvalue ,(->jobject value)))))))
                        
               (j "p.send(m);" `((p ,producer)
                                 (m ,message))))))

  (define hornetq-receive
    (lambda* (consumer (block #f))
        (cond ((and block (boolean? block)) (j "c.receive();" `((c ,consumer))))
              ((and block (number? block)) (j "c.receive(timeout).;" `((c ,consumer)
                                                                                   (timeout ,(->jlong block)))))
              (else (j "c.receiveNoWait();" `((c ,consumer)))))))
  
  (define (hornetq-get-message-jobject m)
    (j "m.getObject();" `((m ,m))))

  (define (hornetq-get-message-object m)
    (->scm-object (hornetq-get-message-jobject m)))

  (define (hornetq-get-message-body-buffer-as-string m)
    (->string (j "m.getCoreMessage().getBodyBuffer().readString();" `((m ,m)))))
  
  (define (hornetq-get-message-properties obj)
    (let ((property-names (iterable->list (j "Collections.list(obj.getPropertyNames());" `((obj ,obj))))))
      (map cons
           (map ->string property-names)
           (pam property-names (lambda (property-name)
                                 (->scm-object (j "o.getObjectProperty(pname);" `((o ,obj) (pname ,property-name)))))))))
  
  (define build-standard-localhost-session-lambda
    (lambda ()
      (memoize (lambda ()
                 (hornetq-session (list (hornetq-transport-configuration "localhost" 5445)))))))
  
  (define build-standard-queue-lambda
    (lambda* ((queue-name "Default"))
      (memoize (lambda ()
                 (hornetq-queue queue-name)))))
  
  (define build-producer-lambda
    (lambda (standard-localhost-session-lambda standard-queue-lambda)
      (memoize (lambda () (hornetq-producer (standard-localhost-session-lambda) (standard-queue-lambda))))))
  
  (define build-consumer-lambda
    (lambda (standard-localhost-session-lambda standard-queue-lambda)
      (memoize (lambda () (hornetq-consumer (standard-localhost-session-lambda) (standard-queue-lambda))))))

  (define (hornetq->work-queue hornetq-consumer work-queue)
    (thread/spawn
     (lambda () 
       (let loop ()
         (work-queue 'put-java (hornetq-receive hornetq-consumer #t))
         (loop)))))

  ;; Example usage.
  ;;  (define localhost-session-lambda (build-standard-localhost-session-lambda))
  ;;  (define standard-queue-lambda (build-standard-queue-lambda))
  ;;  (define p (build-producer-lambda localhost-session-lambda standard-queue-lambda))
  ;;  (define c (build-consumer-lambda localhost-session-lambda standard-queue-lambda))
  ;;  
  ;;  (hornetq-send (localhost-session-lambda) (p) (->jobject (lambda (p) (+ 4 p))))
  ;;  (->scm-object (hornetq-receive (c)))
  
  )
