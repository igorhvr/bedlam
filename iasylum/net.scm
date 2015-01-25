(require-extension (lib iasylum/jcode))

(module iasylum/net
  (http-call-get/string)

  ; (http-call-get/string "http://news.ycombinator.com") retrieves those page contents as a string.
  (define (http-call-get/string destinationUrl)
    (->string
     (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();
        httpget = new org.apache.http.client.methods.HttpGet(destinationurl);
        response = httpclient.execute(httpget);
        result=\"\";
        try {
          tent=response.getEntity();
          result=org.apache.http.util.EntityUtils.toString(tent);
        } catch(Exception e) {
          throw new RuntimeException(e);
        } finally {
          response.close();
        }

        result;"
        `((destinationurl ,(->jstring destinationUrl))))))

  (define (http-call-post-string/string destinationUrl contents)
    (->string
     (j "httpclient = org.apache.http.impl.client.HttpClients.createDefault();
        httppost = new org.apache.http.client.methods.HttpPost(destinationurl);
        httppost.setEntity(new org.apache.http.entity.StringEntity(contents));
        response = httpclient.execute(httppost);
        result=\"\";
        try {
          tent=response.getEntity();
          result=org.apache.http.util.EntityUtils.toString(tent);
        } catch(Exception e) {
          throw new RuntimeException(e);
        } finally {
          response.close();
        }

        result;"
        `((destinationurl ,(->jstring destinationUrl))
          (contents ,(->jstring contents)))
        )))
  )

  
