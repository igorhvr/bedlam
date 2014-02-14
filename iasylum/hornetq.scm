(require-extension (lib iasylum/jcode))


(module iasylum/hornetq
  (hornetq-transport-configuration
   hornetq-session
   hornetq-queue
   hornetq-producer hornetq-consumer
   hornetq-send hornetq-receive
   standard-localhost-session standard-queue p c)

  
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
      session = connection.createSession(false, javax.jms.Session.AUTO_ACKNOWLEDGE);"
       
       `((transportsarray ,(->jarray transports <org.hornetq.api.core.TransportConfiguration>)))))
  
  (define (hornetq-queue queue-name)
    (j "orderQueue = org.hornetq.api.jms.HornetQJMSClient.createQueue(queuename);" `((queuename ,(->jstring queue-name)))))
  
  (define (hornetq-producer session queue)
    (j "session.createProducer(queue);" `((session ,session) (queue ,queue))))
  
  (define (hornetq-consumer session queue)
    (j "session.createConsumer(queue);" `((session ,session) (queue ,queue))))
  
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
  
  (define standard-localhost-session
    (memoize (lambda ()
               (hornetq-session (list (hornetq-transport-configuration "localhost" 5445))))))
  
  (define standard-queue
    (memoize (lambda ()
               (hornetq-queue "Default"))))
  
  (define p (memoize (lambda () (hornetq-producer (standard-localhost-session) (standard-queue)))))
  (define c (memoize (lambda () (hornetq-consumer (standard-localhost-session) (standard-queue)))))
  ;; (hornetq-send (standard-localhost-session) (p) (->jobject (lambda (p) (+ 4 p))))
  ;; (->scm-object (hornetq-receive (c)))
  
  )
