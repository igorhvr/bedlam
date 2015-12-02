(require-extension (lib iasylum/jcode))

(module iasylum/aws
  (aws/make-credentials

   aws/make-sqs-client
   aws/make-sqs-send-message-request
   aws/sqs-send-message aws/sqs-receive-message
   aws/sqs-create-queue

   aws/make-dynamodb-client
   aws/make-dynamodb-attribute aws/ensure-dynamodb-attribute aws/ensure-dynamodb-jmap
   aws/make-dynamodb-put-item-request aws/make-dynamodb-get-item-request aws/make-dynamodb-delete-item-request
   aws/dynamodb-get-item aws/dynamodb-put-item aws/dynamodb-delete-item

   aws/make-dynamodb-simple-eq-key-condition
   aws/make-dynamodb-simple-eq-key-query-request
   aws/dynamodb-simple-eq-key-query

   aws/make-region
   aws/make-s3-client
   aws/s3-put-string
   )
   
   (define (aws/make-credentials access-key secret-key) (j "new com.amazonaws.auth.BasicAWSCredentials(accesskey, secretkey);" `((accesskey ,(->jstring access-key)) (secretkey ,(->jstring secret-key)))))

   (define (aws/make-sqs-client credentials) (j "new com.amazonaws.services.sqs.AmazonSQSClient(credentials);" `((credentials ,credentials))))

   (define (aws/make-sqs-send-message-request queue-url data) (j "new com.amazonaws.services.sqs.model.SendMessageRequest(queueurl, data);" `((queueurl ,(->jstring queue-url)) (data ,(->jstring data)))))

   (define (aws/sqs-send-message sqs-client message) (j "sqs.sendMessage(message);" `((sqs ,sqs-client) (message ,message))))

   (define aws/sqs-receive-message (lambda* (sqs-client queue-url (wait-time-seconds: wait-time-seconds 0)) (->scm-object (j "receiverequest=new com.amazonaws.services.sqs.model.ReceiveMessageRequest(queueurl);if(waittimeseconds!=0)receiverequest.setWaitTimeSeconds(waittimeseconds);result=sqs.receiveMessage(receiverequest.withMaxNumberOfMessages(1)).getMessages();finalresult=null; if (result != null && result.size() > 0) { result=result.get(0); finalresult=result.getBody(); receipt=result.getReceiptHandle(); sqs.deleteMessage(new com.amazonaws.services.sqs.model.DeleteMessageRequest(queueurl, receipt));  } ; finalresult; " `((queueurl ,(->jstring queue-url)) (sqs ,sqs-client) (waittimeseconds ,(->jobject wait-time-seconds)))))))

   ;; DelaySeconds - The time in seconds that the delivery of all messages in the queue will be delayed. An integer from 0 to 900 (15 minutes). The default for this attribute is 0 (zero).
;; MaximumMessageSize - The limit of how many bytes a message can contain before Amazon SQS rejects it. An integer from 1024 bytes (1 KiB) up to 262144 bytes (256 KiB). The default for this attribute is 262144 (256 KiB).
;; MessageRetentionPeriod - The number of seconds Amazon SQS retains a message. Integer representing seconds, from 60 (1 minute) to 1209600 (14 days). The default for this attribute is 345600 (4 days).
;; Policy - The queue's policy. A valid AWS policy. For more information about policy structure, see Overview of AWS IAM Policies in the Amazon IAM User Guide.
;; ReceiveMessageWaitTimeSeconds - The time for which a ReceiveMessage call will wait for a message to arrive. An integer from 0 to 20 (seconds). The default for this attribute is 0.
;; VisibilityTimeout - The visibility timeout for the queue. An integer from 0 to 43200 (12 hours). The default for this attribute is 30. For more information about visibility timeout, see Visibility Timeout in the Amazon SQS Developer Guide.
;; RedrivePolicy - The parameters for dead letter queue functionality of the source queue. For more information about RedrivePolicy and dead letter queues, see Using Amazon SQS Dead Letter Queues in the Amazon SQS Developer Guide.
   (define aws/sqs-create-queue
     (lambda* (sqs-client queue-name (DelaySeconds: DelaySeconds 0) (MaximumMessageSize: MaximumMessageSize 262144) (MessageRetentionPeriod: MessageRetentionPeriod 1209600) (Policy: Policy #f) (ReceiveMessageWaitTimeSeconds: ReceiveMessageWaitTimeSeconds 0) (VisibilityTimeout: VisibilityTimeout 30) (RedrivePolicy: RedrivePolicy #f))
              (let ((parameters-map
                     (->jmap (filter identity
                                     `(("DelaySeconds" . ,(number->string DelaySeconds))
                                       ("MaximumMessageSize" . ,(number->string MaximumMessageSize))
                                       ("MessageRetentionPeriod" . ,(number->string MessageRetentionPeriod))
                                       ,(if Policy `("Policy" . ,(->jobject Policy)) #f)
                                       ("ReceiveMessageWaitTimeSeconds" . ,(number->string ReceiveMessageWaitTimeSeconds))
                                       ("VisibilityTimeout" . ,(number->string VisibilityTimeout))
                                       ,(if RedrivePolicy `("RedrivePolicy" . (->jobject RedrivePolicy)) #f))))))
                (->scm-object
                 (j "url=sqsclient.createQueue(queuename).getQueueUrl();
                     sqsclient.setQueueAttributes(url,attr);
                     url;"
                    `((sqsclient ,sqs-client)
                      (queuename ,(->jstring queue-name))
                      (attr ,parameters-map)))))))
   
   (define (aws/make-dynamodb-client credentials) (j "new com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient(credentials);" `((credentials ,credentials))))

   (define aws/make-dynamodb-attribute (lambda (v) (j "new com.amazonaws.services.dynamodbv2.model.AttributeValue(v);" `((v ,(->jstring v))))))
   
   (define (aws/ensure-dynamodb-attribute v)
     (cond
      ((java-object? v) (if (instance-of v "com.amazonaws.services.dynamodbv2.model.AttributeValue")
                            v
                            (j "new com.amazonaws.services.dynamodbv2.model.AttributeValue(v.toString());" `((v ,v)))))
      (else (aws/ensure-dynamodb-attribute (->jobject v)))))

   (define-java-classes <java.util.concurrent.concurrent-hash-map>)
   (define-generic-java-method |put|)
   (define (aws/ensure-dynamodb-jmap m)
     (let ((result (java-new <java.util.concurrent.concurrent-hash-map>)))
       (for-each
        (match-lambda      
         ((key value) (put result (->jobject key) (aws/ensure-dynamodb-attribute value)))
         ((key . value) (put result (->jobject key) (aws/ensure-dynamodb-attribute value)))
         (#(key value) (put result (->jobject key) (aws/ensure-dynamodb-attribute value))))
        m)
       result))
   
   (define (aws/make-dynamodb-put-item-request table-name string-to-attribute-value-map) (j "new com.amazonaws.services.dynamodbv2.model.PutItemRequest(tablename, stringtoattributevaluemap, \"ALL_OLD\");" `((tablename ,(->jstring table-name)) (stringtoattributevaluemap ,(aws/ensure-dynamodb-jmap string-to-attribute-value-map)))))

   (define aws/make-dynamodb-get-item-request (lambda* (table-name string-to-attribute-value-map (strongly-consistent-read: strongly-consistent-read #t)) (j "new com.amazonaws.services.dynamodbv2.model.GetItemRequest(tablename, stringtoattributevaluemap, cons);" `((tablename ,(->jstring table-name)) (stringtoattributevaluemap ,(aws/ensure-dynamodb-jmap string-to-attribute-value-map)) (cons ,(->jobject strongly-consistent-read))))))

   (define aws/make-dynamodb-delete-item-request
     (lambda* (table-name string-to-attribute-value-map (strongly-consistent-read: strongly-consistent-read #t)) (j "new com.amazonaws.services.dynamodbv2.model.DeleteItemRequest(tablename, stringtoattributevaluemap, \"ALL_OLD\");" `((tablename ,(->jstring table-name)) (stringtoattributevaluemap ,(aws/ensure-dynamodb-jmap string-to-attribute-value-map))))))

   (define (aws/dynamodb-get-item dynamodb-client table-name key-details)
     (->scm-object (j "dyn.getItem(req).getItem();" `((dyn ,dynamodb-client) (req ,(aws/make-dynamodb-get-item-request table-name key-details 'strongly-consistent-read: #t))))))

   (define (aws/dynamodb-put-item dynamodb-client table-name item-details)
     (->scm-object (j "dyn.putItem(req).getAttributes();" `((dyn ,dynamodb-client) (req ,(aws/make-dynamodb-put-item-request table-name item-details))))))

   (define (aws/dynamodb-delete-item dynamodb-client table-name key-details)
     (->scm-object (j "dyn.deleteItem(req).getAttributes();" `((dyn ,dynamodb-client) (req ,(aws/make-dynamodb-delete-item-request table-name key-details))))))

   (define (aws/make-dynamodb-simple-eq-key-condition attribute-name value)
     (let* ((cnd (j "new com.amazonaws.services.dynamodbv2.model.Condition().withComparisonOperator(com.amazonaws.services.dynamodbv2.model.ComparisonOperator.EQ).withAttributeValueList(av);" `((av ,(aws/ensure-dynamodb-attribute value))))))
       (->jmap `((,attribute-name . ,cnd)))))
   
   (define aws/make-dynamodb-simple-eq-key-query-request
     (lambda* (table-name attribute-name attribute-value (strongly-consistent-read: strongly-consistent-read #t))
              (j "new com.amazonaws.services.dynamodbv2.model.QueryRequest(tablename).withKeyConditions(kc).withConsistentRead(consistent);"
                 `((kc ,(aws/make-dynamodb-simple-eq-key-condition attribute-name attribute-value))
                   (tablename ,(->jstring table-name))
                   (consistent ,(->jboolean strongly-consistent-read))))))
   
   ;; Sample usage: (aws/dynamodb-simple-eq-key-query dynamodb-client "dyn-table-name" "id" "c8832de0-bde8-11e4-91a6-56847afe9799")
   (define (aws/dynamodb-simple-eq-key-query dynamodb-client table-name attribute-name attribute-value)
     (map (lambda (individual-map)
            (let ((scheme-map (jmap->alist individual-map)))
              (map (lambda (entry)
                     (match entry ( (k . v) `(,k . ,(->scm-object (j "v.getS();" `((v ,v))))))))
                   scheme-map)))
          (iterable->list
           (j "dyn.query(req).getItems();" `((dyn ,dynamodb-client)
                                             (req ,(aws/make-dynamodb-simple-eq-key-query-request table-name attribute-name attribute-value
                                                                                                  'strongly-consistent-read: #t)))))))

   ;; Valid regions (as of 2015-12-01) 
   ;; AP_NORTHEAST_1 / AP_SOUTHEAST_1 / AP_SOUTHEAST_2 / CN_NORTH_1 / EU_CENTRAL_1
   ;; EU_WEST_1 / GovCloud / SA_EAST_1 / US_EAST_1 / US_WEST_1 / US_WEST_2 
   (define (aws/make-region region-name)
     (j "com.amazonaws.regions.Regions.valueOf(regionname);" `((regionname ,(->jstring region-name)))))

   (define aws/make-s3-client
     (lambda* (credentials (region: region #f))
              (let ((result (j "ns3c=new com.amazonaws.services.s3.AmazonS3Client(cred);" `((cred ,credentials)))))
                (when region (j "s3client.setRegion(region);" `((s3client ,result) (region ,region))))
                result)))

   (define aws/s3-put-string
     (lambda* (s3-client bucket object string (public-read: public-read #f))
              (let ((aclv (if public-read
                              (j "com.amazonaws.services.s3.model.CannedAccessControlList.PublicRead")
                              (j "com.amazonaws.services.s3.model.CannedAccessControlList.Private"))))
                (j "omd = new com.amazonaws.services.s3.model.ObjectMetadata();                   
                    s3.putObject(new com.amazonaws.services.s3.model.PutObjectRequest(bucket, objname, fl, omd).withCannedAcl(aclv));"
                   `((bucket ,(->jstring bucket)) (objname ,(->jstring object)) (fl ,(string->java.io.InputStream string)) (s3 ,s3-client) (aclv ,aclv))))))

                  
   
)
