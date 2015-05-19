(require-extension (lib iasylum/jcode))

(module iasylum/aws
  (aws/make-credentials

   aws/make-sqs-client
   aws/make-sqs-send-message-request
   aws/sqs-send-message aws/sqs-receive-message

   aws/make-dynamodb-client
   aws/make-dynamodb-attribute aws/ensure-dynamodb-attribute aws/ensure-dynamodb-jmap
   aws/make-dynamodb-put-item-request aws/make-dynamodb-get-item-request aws/make-dynamodb-delete-item-request
   aws/dynamodb-get-item aws/dynamodb-put-item aws/dynamodb-delete-item

   aws/make-dynamodb-simple-eq-key-condition
   aws/make-dynamodb-simple-eq-key-query-request
   aws/dynamodb-simple-eq-key-query

   aws/make-s3-client
   aws/s3-put-string
   )
   
   (define (aws/make-credentials access-key secret-key) (j "new com.amazonaws.auth.BasicAWSCredentials(accesskey, secretkey);" `((accesskey ,(->jstring access-key)) (secretkey ,(->jstring secret-key)))))

   (define (aws/make-sqs-client credentials) (j "new com.amazonaws.services.sqs.AmazonSQSClient(credentials);" `((credentials ,credentials))))

   (define (aws/make-sqs-send-message-request queue-url data) (j "new com.amazonaws.services.sqs.model.SendMessageRequest(queueurl, data);" `((queueurl ,(->jstring queue-url)) (data ,(->jstring data)))))

   (define (aws/sqs-send-message sqs-client message) (j "sqs.sendMessage(message);" `((sqs ,sqs-client) (message ,message))))

   (define aws/sqs-receive-message (lambda* (sqs-client queue-url (wait-time-seconds: wait-time-seconds 0)) (->scm-object (j "receiverequest=new com.amazonaws.services.sqs.model.ReceiveMessageRequest(queueurl);if(waittimeseconds!=0)receiverequest.setWaitTimeSeconds(waittimeseconds);result=sqs.receiveMessage(receiverequest.withMaxNumberOfMessages(1)).getMessages();finalresult=null; if (result != null && result.size() > 0) { result=result.get(0); finalresult=result.getBody(); receipt=result.getReceiptHandle(); sqs.deleteMessage(new com.amazonaws.services.sqs.model.DeleteMessageRequest(queueurl, receipt));  } ; finalresult; " `((queueurl ,(->jstring queue-url)) (sqs ,sqs-client) (waittimeseconds ,(->jobject wait-time-seconds)))))))
   
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
   (define (aws/make-s3-client credentials)
     (j "new com.amazonaws.services.s3.AmazonS3Client(cred);" `((cred ,credentials))))

   (define aws/s3-put-string
     (lambda* (s3-client bucket object string (public-read: public-read #f))
              (let ((aclv (if public-read "public-read" "private")))
                (j "omd = new com.amazonaws.services.s3.model.ObjectMetadata();
                    omd.addUserMetadata(\"acl\", aclv);
                    s3.putObject(bucket, objname, fl, omd);"
                   `((bucket ,(->jstring bucket)) (objname ,(->jstring object)) (fl ,(string->java.io.InputStream string)) (s3 ,s3-client) (aclv ,(->jstring aclv)))))))

                  
   
)
