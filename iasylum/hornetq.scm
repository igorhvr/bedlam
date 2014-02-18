(require-extension (lib iasylum/jcode))


(module iasylum/hornetq
  (hornetq-transport-configuration
   hornetq-session
   hornetq-queue
   hornetq-producer hornetq-consumer
   hornetq-send hornetq-receive
   build-standard-localhost-session-lambda
   build-standard-queue-lambda
   build-producer-lambda
   build-consumer-lambda
   )

  
  (define hornetq-transport-configuration
    (lambda* (host (port 5445))
      (j "map = new java.util.concurrent.ConcurrentHashMap();
          map.put(\"host\", myhost);
          map.put(\"port\", myport);
          new org.hornetq.api.core.TransportConfiguration(org.hornetq.core.remoting.impl.netty.NettyConnectorFactory.class.getName(), map);"
         `((myhost ,(->jstring host))
           (myport ,(->jint port))))))
  
  (define (hornetq-session transports)
    (define-java-class <org.hornetq.api.core.TransportConfiguration> |org.hornetq.api.core.TransportConfiguration|)
    
    (j "cf = org.hornetq.api.jms.HornetQJMSClient.createConnectionFactoryWithHA(org.hornetq.api.jms.JMSFactoryType.CF, transportsarray);
      connection = cf.createConnection();
      connection.start();
      session = connection.createSession(false, javax.jms.Session.DUPS_OK_ACKNOWLEDGE);"
       
       `((transportsarray ,(->jarray transports <org.hornetq.api.core.TransportConfiguration>)))))
  
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
  
  (define (hornetq-send session producer message)  
    (j "p.send(m);" `((p ,producer)
                      (m ,(j "r=session.createObjectMessage();
                            r.setObject(message);
                            r;" `((session ,session) (message ,message)))))))
  
  (define hornetq-receive
    (lambda* (consumer (block #f))
        (cond ((and block (boolean? block)) (j "c.receive().getObject();" `((c ,consumer))))
              ((and block (number? block)) (j "c.receive(timeout).getObject();" `((c ,consumer)
                                                                                   (timeout ,(->jlong block)))))
              (else (j "c.receiveNoWait().getObject();" `((c ,consumer)))))))
  
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

  ;; Example usage.
  ;;  (define localhost-session-lambda (build-standard-localhost-session-lambda))
  ;;  (define standard-queue-lambda (build-standard-queue-lambda))
  ;;  (define p (build-p-lambda localhost-session-lambda standard-queue-lambda))
  ;;  (define c (build-c-lambda localhost-session-lambda standard-queue-lambda))
  ;;  
  ;;  (hornetq-send (localhost-session-lambda) (p) (->jobject (lambda (p) (+ 4 p))))
  ;;  (->scm-object (hornetq-receive (c)))
  
  )
