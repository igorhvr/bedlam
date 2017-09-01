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
   aws/make-dynamodb-unique-put-item
   aws/dynamodb-get-item aws/dynamodb-put-item aws/dynamodb-delete-item

   aws/make-dynamodb-simple-eq-key-condition
   aws/make-dynamodb-simple-eq-key-query-request
   aws/dynamodb-simple-eq-key-query

   aws/make-region
   aws/make-s3-client

   aws/s3-put-binary
   aws/s3-put-string
   aws/s3-put-file

   aws/s3-make-allow-GET-POST-from-anywhere-CORS-rules
   aws/s3-set-CORS
   aws/s3-create-bucket
   aws/s3-delete-bucket
   aws/s3-delete-bucket-recursively
   aws/s3-blow-up-s3-and-regret-tears-of-blood-I-am-oficially-insane
   aws/s3-list-buckets
   aws/s3-list-buckets-names
   aws/s3-set-bucket-website-configuration
   aws/s3-set-bucket-versioning-configuration
   aws/s3-add-bucket-autoerasing-rule
   aws/s3-make-storage-class
   aws/s3-change-object-storage-class

   aws/iam-client
   aws/iam-create-role
   aws/iam-attach-role-policy

   aws/make-lambda-client
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

   (define (aws/make-dynamodb-unique-put-item dynamodb-client table-name key)
     (let* ((value-var (string-append* "v" (substring (sha256 (gensym)) 0 20)))
            (value-var2 (string-append* "v" (substring (sha256 (gensym)) 0 20)))
            (themap-var1 (string-append* "v" (substring (sha256 (gensym)) 0 20)))
            (themap-var2 (string-append* "v" (substring (sha256 (gensym)) 0 20)))
            (thekey (string-append* "v" (substring (sha256 (gensym)) 0 20)))
            (att-value (j (format "new com.amazonaws.services.dynamodbv2.model.AttributeValue(~a);" thekey)
                          `((,thekey ,(->jstring key)))))
            (att-value2 (j "new com.amazonaws.services.dynamodbv2.model.ExpectedAttributeValue(false);")))
       (let ((request
              (j (format "new com.amazonaws.services.dynamodbv2.model.PutItemRequest().withTableName(tablename).withItem(~a).withExpected(~a);" themap-var1 themap-var2)
                 `((tablename ,(->jstring table-name))
                   (,themap-var1 ,(j "java.util.Collections.singletonMap(\"nonce\", v);" `((v ,att-value))))
                   (,themap-var2 ,(j "java.util.Collections.singletonMap(\"nonce\", v2);" `((v2 ,att-value2))))))))
         (->scm-object (j "dyn.putItem(req).getAttributes();"
                          `((dyn ,dynamodb-client)
                            (req ,request)))))))
     
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
     (j "com.amazonaws.regions.Region.getRegion(com.amazonaws.regions.Regions.valueOf(regionname));" `((regionname ,(->jstring region-name)))))

   (define aws/make-s3-client
     (lambda* (credentials (region: region #f))
              (let ((result (j "ns3c=new com.amazonaws.services.s3.AmazonS3Client(cred);" `((cred ,credentials)))))
                (when region (j "s3client.setRegion(region);" `((s3client ,result) (region ,region))))
                result)))

   ;; TODO: Use http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/ClientConfiguration.html#setMaxErrorRetry(int)
   ;; somehow to be able to configure retry behavior when uploading data to s3. See if we can expand this to other stuff.
   (define aws/s3-put-binary
     (lambda* (s3-client bucket object byte-array-input-stream
               (mime-type: mime-type #f)
               (char-encoding: char-encoding "utf-8")
               (max-age: max-age 300)
               (public-read: public-read #f)
               (reduced-redundancy: reduced-redundancy #f))
              (let* ((aclv (if public-read
                               (j "com.amazonaws.services.s3.model.CannedAccessControlList.PublicRead")
                               (j "com.amazonaws.services.s3.model.CannedAccessControlList.Private")))
                     (omd (j "new com.amazonaws.services.s3.model.ObjectMetadata();"))                    
                     (bytearray (j "org.apache.commons.io.IOUtils.toByteArray(is);" `((is ,byte-array-input-stream))))
                     (length (->scm-object (j "ba.length" `((ba ,bytearray))))))
                (when reduced-redundancy (j "omd.setHeader(\"x-amz-storage-class\", \"REDUCED_REDUNDANCY\");" `((omd ,omd))))
                (when mime-type (j "omd.setContentType(ctype);" `((omd ,omd)
                                                                  (ctype ,(->jstring mime-type)))))
                (when max-age (j "omd.setCacheControl(mage);" `((omd ,omd)
                                                                (mage ,(->jstring (format "max-age=~a" max-age))))))
                (j "omd.setContentLength(lgt);" `((lgt ,(->jlong length))))
                (j "is.reset();" `((is ,byte-array-input-stream)))
                (j "s3.putObject(new com.amazonaws.services.s3.model.PutObjectRequest(bucket, objname, fl, omd).withCannedAcl(aclv));"
                   `((bucket ,(->jstring bucket))
                     (objname ,(->jstring object))
                     (omd ,omd)
                     (fl ,byte-array-input-stream)
                     (s3 ,s3-client)
                     (aclv ,aclv))))))

   (define aws/s3-put-file
     (lambda* (s3-client bucket object file-full-path
               (mime-type: mime-type #f)
               (char-encoding: char-encoding "utf-8")
               (max-age: max-age 300)
               (public-read: public-read #f)
               (reduced-redundancy: reduced-redundancy #f))
              (aws/s3-put-binary s3-client bucket object
                                 (j "new java.io.ByteArrayInputStream(org.apache.commons.io.FileUtils.readFileToByteArray(new java.io.File(filename)));"
                                    `((filename ,(->jstring file-full-path))))
                                 'mime-type: mime-type
                                 'max-age: max-age
                                 'public-read: public-read
                                 'reduced-redundancy: reduced-redundancy)))

   (define aws/s3-put-string
     (lambda* (s3-client bucket object string
               (mime-type: mime-type #f)
               (char-encoding: char-encoding "utf-8")
               (max-age: max-age 300)
               (public-read: public-read #f)
               (reduced-redundancy: reduced-redundancy #f))
              (aws/s3-put-binary s3-client bucket object
                                 (j "new java.io.ByteArrayInputStream(str.getBytes(encoding))"
                                    `((encoding ,(->jstring
                                                  (string-upcase char-encoding)))
                                      (str ,(->jstring string))))
                                 'mime-type: mime-type
                                 'max-age: max-age
                                 'public-read: public-read
                                 'reduced-redundancy: reduced-redundancy)))

   (define (aws/s3-make-allow-GET-POST-from-anywhere-CORS-rules)
     (j "rule1 = new com.amazonaws.services.s3.model.CORSRule()
        .withId(corid)
        .withAllowedHeaders(Arrays.asList(new String[] { \"*\" }))
        .withMaxAgeSeconds(3000)
        .withAllowedMethods(Arrays.asList(new com.amazonaws.services.s3.model.CORSRule.AllowedMethods[] { 
            com.amazonaws.services.s3.model.CORSRule.AllowedMethods.GET, com.amazonaws.services.s3.model.CORSRule.AllowedMethods.POST}))
        .withAllowedOrigins(Arrays.asList(new String[] { \"*\" }));
         Arrays.asList(new com.amazonaws.services.s3.model.CORSRule[] {rule1});"
        `((corid ,(->jstring (uuid-string))))))


   (define (aws/s3-set-CORS s3-client bucket-name rules)
     (j 
      "configuration = new com.amazonaws.services.s3.model.BucketCrossOriginConfiguration();
       configuration.setRules(rules);
       client.setBucketCrossOriginConfiguration(bucketname, configuration);"
      `((client ,s3-client)
        (bucketname ,(->jstring bucket-name))
        (rules ,rules))))
   
   (define (aws/s3-create-bucket s3-client name)
     (j "s3client.createBucket(new com.amazonaws.services.s3.model.CreateBucketRequest(bucketname));" `((bucketname ,(->jstring name)) (s3client ,s3-client))))

   (define (aws/s3-delete-bucket s3-client name)
     (j "s3client.deleteBucket(new com.amazonaws.services.s3.model.DeleteBucketRequest(bucketname));" `((bucketname ,(->jstring name)) (s3client ,s3-client))))
   
   (define (aws/s3-delete-bucket-recursively s3-client name)
     (j "while(true) {
            objs=s3client.listObjects(new com.amazonaws.services.s3.model.ListObjectsRequest().withMaxKeys(maxkeys).withBucketName(bucketname)).getObjectSummaries();
            fres=new java.util.ArrayList();
            it=objs.iterator();
            if(!it.hasNext()) break; // Finally empty.
            while(it.hasNext()) {
              nv=it.next();
              fres.add(new com.amazonaws.services.s3.model.DeleteObjectsRequest.KeyVersion(nv.getKey()));
            }
            s3client.deleteObjects(new com.amazonaws.services.s3.model.DeleteObjectsRequest(bucketname).withKeys(fres));
         }"
        `((maxkeys ,(->jint 512))
          (bucketname ,(->jstring name))
          (s3client ,s3-client)))
     
     (aws/s3-delete-bucket s3-client name))

   (define aws/s3-blow-up-s3-and-regret-tears-of-blood-I-am-oficially-insane
     (lambda* (s3-client (i-understand-that-this-will-OBLITERATE-every-s3-bucket-accessible-by-this-s3-client: go-ahead #f))
              (if go-ahead
                  (begin
                    (d/n "I hope you know what you are doing. Waiting 10 seconds before starting...")
                    (sleep-seconds 13)
                    (map (lambda (element) (aws/s3-delete-bucket-recursively
                                            s3-client
                                            element))
                         (aws/s3-list-buckets-names s3-client )))
                  (d/n "If you really want to do this pass the 'i-understand-that-this-will-OBLITERATE-every-s3-bucket-accessible-by-this-s3-client: parameter as #t."))))

   (define (aws/s3-list-buckets s3-client)
     (j "s3client.listBuckets(new com.amazonaws.services.s3.model.ListBucketsRequest());" `((s3client ,s3-client))))

   (define (aws/s3-list-buckets-names s3-client)
     (map (lambda (v) (->string (j "v.getName();" `((v ,v))))) (iterable->list (aws/s3-list-buckets s3-client ))))
   
   (define (aws/s3-set-bucket-website-configuration s3-client bucket-name index-doc error-doc)
     (j "s3client.setBucketWebsiteConfiguration(bucketname, new com.amazonaws.services.s3.model.BucketWebsiteConfiguration(indexdoc, errordoc));"
        `((indexdoc ,(->jstring index-doc)) (errordoc ,(->jstring error-doc)) (bucketname ,(->jstring bucket-name))
          (s3client ,s3-client))))

   (define (aws/s3-set-bucket-versioning-configuration s3-client bucket-name enable?)
     (assert enable?) ; Disabling not yet implemented.
     (j "bvc = new com.amazonaws.services.s3.model.BucketVersioningConfiguration().withStatus(\"Enabled\");
         s3client.setBucketVersioningConfiguration(new com.amazonaws.services.s3.model.SetBucketVersioningConfigurationRequest(bucketname, bvc));"
        `((bucketname ,(->jstring bucket-name))
          (s3client ,s3-client))))
   
   (define (aws/s3-add-bucket-autoerasing-rule s3-client bucket-name id days prefix)
     (j "configuration=s3client.getBucketLifecycleConfiguration(bucketname);
      if(configuration==null) configuration=new com.amazonaws.services.s3.model.BucketLifecycleConfiguration();
      newrule=new com.amazonaws.services.s3.model.BucketLifecycleConfiguration.Rule()
                        .withId(id)
                        .withPrefix(prefix)
                        .withExpirationInDays(days)
                        .withStatus(com.amazonaws.services.s3.model.BucketLifecycleConfiguration.ENABLED.toString());
      if(configuration.getRules()==null) {
         configuration.setRules(
                    Arrays.asList(
                      new com.amazonaws.services.s3.model.BucketLifecycleConfiguration.Rule[] {newrule}));
      } else {
         configuration.getRules().add(newrule);
      }
      s3client.setBucketLifecycleConfiguration(bucketname, configuration);"
        `((prefix ,(->jstring prefix))
          (id ,(->jstring id))
          (days ,(->jobject days))
          (bucketname ,(->jstring bucket-name))
          (s3client ,s3-client))))

   ;; Valid storage classes (as of 2015-12-05) 
   ;; Glacier / ReducedRedundancy / Standard / StandardInfrequentAccess
   (define (aws/s3-make-storage-class storage-class)
     (j "com.amazonaws.services.s3.model.StorageClass(v);" `((v ,(->jstring storage-class)))))
   
   (define (aws/s3-change-object-storage-class s3-client bucket-name key-name storage-class)
     (j 
      "client.changeObjectStorageClass(bucketname, keyname, storageclass);"
      `((client ,s3-client)
        (bucketname ,(->jstring bucket-name))
        (keyname ,(->jstring key-name))
        (storageclass ,storage-class))))


   (define (aws/iam-client credentials) (j "new com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient(credentials);" `((credentials ,credentials))))

   (define aws/iam-create-role
     (lambda* (iam-client (assume-role-policy-document: assume-role-policy-document) (path: path "/") (role-name: role-name))
              (let ((result (j "iamclient.createRole(
                                                   new com.amazonaws.services.identitymanagement.model.CreateRoleRequest().
                                                     withAssumeRolePolicyDocument(apd).
                                                     withPath(pt).
                                                     withRoleName(rn)).getRole();" `((iamclient ,iam-client)
                                                                                                (apd ,(->jstring assume-role-policy-document))
                                                                                                (pt ,(->jstring path))
                                                                                                (rn ,(->jstring role-name))))))
                `((path . ,(->scm-object (j "r.getPath();" `((r ,result)))))
                  (role-name . ,(->scm-object (j "r.getRoleName();" `((r ,result)))))
                  (role-id . ,(->scm-object (j "r.getRoleId();" `((r ,result)))))
                  (arn . ,(->scm-object (j "r.getArn();" `((r ,result)))))
                  (create-date . ,(->scm-object (j "r.getCreateDate();" `((r ,result)))))
                  (assume-role-policy-document . ,(->scm-object (j "r.getAssumeRolePolicyDocument();" `((r ,result)))))))))

   (define aws/iam-attach-role-policy
     (lambda* (iam-client (role-name: role-name) (policy-arn: policy-arn))
              (j "iamclient.attachRolePolicy(new com.amazonaws.services.identitymanagement.model.AttachRolePolicyRequest().withPolicyArn(pa).withRoleName(rn));"
                 `((iamclient ,iam-client)
                   (pa ,(->jstring policy-arn))
                   (rn ,(->jstring role-name))))))
 

   (define (aws/make-lambda-client credentials) (j "new com.amazonaws.services.lambda.AWSLambdaClient(credentials);" `((credentials ,credentials))))

   (define aws/lambda-create-function
     (lambda* (lambda-client
               (function-zip-file: function-zip-file) ; The code for the Lambda function.
               (function-name: function-name) ; The name you want to assign to the function you are uploading.
               (function-description: function-description function-name) ; A short, user-defined function description.
               (handler: handler #f) ; The function within your code that Lambda calls to begin execution.
               (memory-size: memory-size #f) ; The amount of memory, in MB, your Lambda function is given.
               (publish: publish #t) ; This boolean parameter can be used to request AWS Lambda to create the Lambda function and publish a version as an atomic operation.
               (IAM-role-ARN: role) ; The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it executes your function to access any other Amazon Web Services (AWS) resources.
               (runtime: runtime) ; The runtime environment for the Lambda function you are uploading. Allowed values: nodejs, nodejs4.3, java8, python2.7
               (timeout-seconds: timeout 3) ; The function execution time at which Lambda should terminate the function. 
               (vpc-config: vpc-config #f) ; If your Lambda function accesses resources in a VPC, you provide this parameter identifying the list of security group IDs and subnet IDs. These must belong to the same VPC. You must provide at least one security group and one subnet ID.
               )
              (and-let* ((bytebuffer-thunk (make-future (lambda () (java.io.File->java.nio.ByteBuffer function-zip-file))))
                         (function-code-thunk (make-future (lambda () (j "new com.amazonaws.services.lambda.model.FunctionCode().withZipFile(bytebufferp);" `((bytebufferp ,(bytebuffer-thunk)))))))
                         (create-function-request (j "new com.amazonaws.services.lambda.model.CreateFunctionRequest();"))
                         ((begin
                            (j "cfr.setFunctionName(functionname);" `((functionname ,(->jstring function-name)) (cfr ,create-function-request)))
                            (j "cfr.setDescription(desc);" `((desc ,(->jstring function-description)) (cfr ,create-function-request)))
                            (when handler ((j "cfr.setHandler(handl);" `((handl ,(->jstring handler)) (cfr ,create-function-request)))))
                            (when memory-size ((j "cfr.setMemorySize(mem);" `((mem ,(->jint memory-size)) (cfr ,create-function-request)))))
                            (j "cfr.setPublish(pub);" `((pub ,(->jboolean publish)) (cfr ,create-function-request)))
                            (j "cfr.setRole(role);" `((role ,(->jstring role)) (cfr ,create-function-request)))
                            (j "cfr.setRuntime(role);" `((runtime ,(->jstring runtime)) (cfr ,create-function-request)))
                            (j "cfr.setTimeout(tmt);" `((tmt ,(->jint timeout)) (cfr ,create-function-request)))
                            (when vpc-config (j "cfr.setVpcConfig(vpcconfig);" `((vpccofig ,vpc-config) (cfr ,create-function-request))))
                            #t)))
                (d/n "THIS IS WORK IN PROGRESS.")
                (j "lambdaclient.createFunction(cfr.withCode(functioncode));" `((cfr ,create-function-request)
                                                                                (functioncode ,(function-code-thunk))
                                                                                (lambdaclient ,lambda-client))))))
)


              
