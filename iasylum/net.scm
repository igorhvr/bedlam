(require-extension (lib iasylum/jcode))

(module iasylum/net
  (http-call-get/string
   http-call-get-headers/string 
   http-call-post-string/string
   http-call-get-status-code/string
   )

  ; (http-call-get/string "http://news.ycombinator.com") retrieves those page contents as a string.
  (define (http-call-get/string destinationUrl)
    (let ((java-result
     (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();
        httpget = new org.apache.http.client.methods.HttpGet(destinationurl);
        response = httpclient.execute(httpget);
        result=\"\";
        try {
          tent=response.getEntity();
          result=(tent!=null)?org.apache.http.util.EntityUtils.toString(tent):null;
        } catch(Exception e) {
          throw new RuntimeException(e);
        } finally {
          response.close();
        }

        result;"
        `((httpclient)(httpget)(response)(result)
          (destinationurl ,(->jstring destinationUrl))))))
      (if (java-null? java-result) #f (->string java-result))))
      

  ;;Returns the status code from a get request
  (define (http-call-get-status-code/string destinationUrl)
    (->string
     (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();
        httpget = new org.apache.http.client.methods.HttpGet(destinationurl);
        response = httpclient.execute(httpget);
        status=\"\";
        try {
          tent=response.getStatusLine();
          status=tent.getStatusCode();
        } catch(Exception e) {
          throw new RuntimeException(e);
        } finally {
          response.close();
        }

        status;"
        `((httpclient)(httpget)(response)(result)
          (destinationurl ,(->jstring destinationUrl))))))

  ;; Implements headers to GET requests

  (define http-call-get-headers/string 
    (lambda* (destinationUrl (headers: headers #f) (max-size-bytes: max-size-bytes #f))
             (let ((httpclient (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();"
                               `((httpclient))))
                   (httpget (j "httpget = new org.apache.http.client.methods.HttpGet(destinationurl);
                                httpget;"
                               `((httpget)
                                 (destinationurl ,(->jstring destinationUrl))))))

               (when headers
                 (map (lambda (v)  (match-let ( ( (vname vvalue) v ) )
                                         (j "httpget.addHeader(hn, hv);"`((hn ,(->jstring vname)) (hv ,(->jstring vvalue)) (httpget ,httpget)))
                                         )) headers))
               
               (let ((java-result (j "response = httpclient.execute(httpget);
                                      result=\"\";
                                      try {
                                        tent=response.getEntity();
                                        if((tent != null) && ( (!hasmaxsize) || ((tent.getContentLength() > -1) && (tent.getContentLength() <= maxsizebytes)))) {
                                         result=org.apache.http.util.EntityUtils.toString(tent);
                                        } else {
                                         result=null;
                                        }
                                      } catch(Exception e) {
                                        throw new RuntimeException(e);
                                      } finally {
                                        response.close();
                                      }
                                
                                      result;"
                                     `((response)(result)(tent)(httpget ,httpget)(httpclient ,httpclient)
                                       (hasmaxsize ,(->jboolean (if max-size-bytes #t #f))) (maxsizebytes ,(->jlong (or max-size-bytes 0)))
                                       ))))
                 (if (java-null? java-result) #f (->string java-result)))
               )))
  
  ;; TODO file upload - http://stackoverflow.com/questions/1067655/how-to-upload-a-file-using-java-httpclient-library-working-with-php
  ;; TODO Use code similar to
  ;; HttpClient httpClient = HttpClients.custom().setDefaultRequestConfig(
  ;; RequestConfig.custom().setCookieSpec(CookieSpecs.STANDARD).build()).build();
  ;; to deal with Invalid cookie header errors.
  (define http-call-post-string/string
    (lambda* (destinationUrl contents (headers: headers '()))
             (let ((httpclient (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();" `((httpclient))))
                   (httppost (j "httppost = new org.apache.http.client.methods.HttpPost(destinationurl);
                                 httppost;"
                                `((httppost)(destinationurl ,(->jstring destinationUrl))))))
               
               (map (lambda (v)  (match-let ( ( (vname vvalue) v ) )
                                            (j "httppost.addHeader(hn, hv);"`((hn ,(->jstring vname)) (hv ,(->jstring vvalue)) (httppost ,httppost)))
                                            )) headers)
                                              
             (let ((java-result
               (j "httppost.setEntity(new org.apache.http.entity.StringEntity(contents));
                  response = httpclient.execute(httppost);
                  result=\"\";
                  try {
                    tent=response.getEntity();
                    result=(tent!=null)?org.apache.http.util.EntityUtils.toString(tent):null;
                  } catch(Exception e) {
                    throw new RuntimeException(e);
                  } finally {
                    response.close();
                  }
          
                  result;"
                  `((response)(result)(tent)
                    (contents ,(->jstring contents))
                    (httppost ,httppost)
                    (httpclient ,httpclient)))))
               (if (java-null? java-result) #f (->string java-result))))))

)
