;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.
(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/log))

;; Sends an email according to parameters.
;; Depends on:
;;    activation-1.1.jar  commons-email-1.4.jar  mail.jar
;;    iasylum/jcode
(module iasylum/email
  (send-email
   pump-imaps-inbox-to-work-queue
   test-pump-imaps-inbox-to-work-queue)


  ;;(send-email
  ;;  'smtp-server:  "smtp.test.net"
  ;;  'to:  '(("j@t.net" ""))
  ;;  'sender-email:  "s@s.net"
  ;;  'sender-name:  "s"
  ;;  'cc:  '(("w@w.net" "w"))
  ;;  'subject:  "bla"
  ;;  'use-ssl:  #t
  ;;  'authentication-login:  "s@s.net"
  ;;  'authentication-password:  "fk*fjlanP"
  ;;  'message-text:  "Uamsg"
  ;; )
  (define send-email
    (lambda* ((smtp-server: mailserver "localhost")
         (to: to #f)
         (cc: cc #f)
         (bcc: bcc #f)
         (sender-email: senderemail) (sender-name: sendername senderemail)
         (authentication-login: authentication-login "") (authentication-password: authentication-password "")
         (use-ssl: use-ssl #t) (use-starttls: use-starttls #f)         
         (smtp-port: smtp-port 25)
         (ssl-smtp-port: ssl-smtp-port 465)
         (subject: subject "") (message-text: messagetext "=")
         (attachments-file-path: attachments-file-path #f))
      (assert (or (not to) (list? to))) (assert (or (not cc) (list? cc)))

      (let ((email
             (j 
               "import org.apache.commons.mail.MultiPartEmail;
                email = new MultiPartEmail();
                email.setCharset(\"utf-8\");
                email.setSmtpPort(smtpport);
                email.setSslSmtpPort(sslsmtpport);
                email.setHostName(mailserver);
                email;"
               `((mailserver ,(->jstring mailserver))
                 (usessl ,(->jboolean use-ssl))
                 (smtpport ,(->jint smtp-port))
                 (sslsmtpport ,(->jstring (number->string ssl-smtp-port)))
                 ))))

        (when attachments-file-path
          (for-each
           (lambda (filepath)
             (j "email.attach(new java.io.File(filepath));"
                `((email ,email)(filepath ,(->jstring filepath)))))
           attachments-file-path))
        
        (when to
          (for-each (match-lambda ((vemail vname)
                              (j "email.getToAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));"
                                 `((email ,email) (lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                             (vemail
                              (j "email.getToAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));"
                                 `((email ,email) (lccemail ,(->jstring vemail))  )))
                             )
                    to))
        
        (when cc
          (for-each (match-lambda ((vemail vname)
                              (j "email.getCcAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));"
                                 `((email ,email) (lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                             (vemail
                              (j "email.getCcAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));"
                                 `((email ,email) (lccemail ,(->jstring vemail))  )))
                             )
                    cc))

        (when bcc
          (for-each (match-lambda ((vemail vname)
                              (j "email.getBccAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));"
                                 `((email ,email) (lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                             (vemail
                              (j "email.getBccAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));"
                                 `((email ,email) (lccemail ,(->jstring vemail)) )))
                             )
                    bcc))
      
        (j "
            if(!\"\".equals(authenticationlogin)) {
                email.setAuthentication(authenticationlogin, authenticationpassword);
            }
            email.setSSL(usessl);
            email.setStartTLSEnabled(usestarttls);
            email.setFrom(senderemail, sendername);
            email.setSubject(subject);
            email.setMsg(messagetext);
            email.send();"
           `((email ,email)
             (mailserver ,(->jstring mailserver))
             (subject ,(->jstring subject))
             (messagetext ,(->jstring messagetext))
             (usessl ,(->jboolean use-ssl))
             (usestarttls ,(->jboolean use-starttls))
             (authenticationlogin ,(->jstring authentication-login))
             (authenticationpassword ,(->jstring authentication-password))
             (senderemail ,(->jstring senderemail))
             (sendername ,(->jstring sendername))
             )))))

  ;; Fetches messages from an IMAPS server and puts them into a work queue
  ;; Returns the number of messages processed
  ;; Messages are deleted from the server after being put in the queue by default
  (define pump-imaps-inbox-to-work-queue
    (lambda* ((imaps-server-hostname: hostname)
              (username: username)
              (password: password)
              (work-queue: queue)
              (max-messages: max-messages #f)
              (delete-messages: delete-messages #t))
      
      (let* ((messages 
              (j (quote-convert "//J'
                 import java.util.*;
                 import javax.mail.*;
                 
                 // Setup properties and connect to the IMAPS server
                 Properties props = new Properties();
                 Session session = Session.getInstance(props, null);
                 Store store = session.getStore(\"imaps\");
                 store.connect(hostname, username, password);
                 
                 // Open INBOX folder
                 Folder inbox = store.getFolder(\"INBOX\");
                 inbox.open(Folder.READ_WRITE);
                 int messageCount = inbox.getMessageCount();
                 
                 // Limit messages if max-messages is specified
                 int maxmessagesValue = maxmessages;
                 if (maxmessagesValue > 0 && messageCount > maxmessagesValue) {
                     messageCount = maxmessagesValue;
                 }
                 
                 List messages = new ArrayList();
                 
                 // Process all messages
                 for (int i = 1; i <= messageCount; i++) {
                     try {
                         Message msg = inbox.getMessage(i);
                         
                         // Create a map to store message details
                         Map messageData = new HashMap();
                         
                         // Extract basic message info
                         messageData.put(\"subject\", msg.getSubject() != null ? msg.getSubject() : \"\");
                         messageData.put(\"sent-date\", msg.getSentDate());
                         messageData.put(\"received-date\", msg.getReceivedDate());
                         String[] messageIds = msg.getHeader(\"Message-ID\");
                         messageData.put(\"message-id\", messageIds != null && messageIds.length > 0 ? messageIds[0] : \"\");
                         
                         // Handle From addresses
                         Address[] fromAddresses = msg.getFrom();
                         List fromList = new ArrayList();
                         if (fromAddresses != null) {
                             for (Address address : fromAddresses) {
                                 fromList.add(address.toString());
                             }
                         }
                         messageData.put(\"from\", fromList);
                         
                         // Handle To addresses
                         Address[] toAddresses = msg.getRecipients(Message.RecipientType.TO);
                         List toList = new ArrayList();
                         if (toAddresses != null) {
                             for (Address address : toAddresses) {
                                 toList.add(address.toString());
                             }
                         }
                         messageData.put(\"to\", toList);
                         
                         // Handle CC addresses
                         Address[] ccAddresses = msg.getRecipients(Message.RecipientType.CC);
                         List ccList = new ArrayList();
                         if (ccAddresses != null) {
                             for (Address address : ccAddresses) {
                                 ccList.add(address.toString());
                             }
                         }
                         messageData.put(\"cc\", ccList);
                         
                         // Extract content
                         Object content = null;
                         try {
                             content = msg.getContent();
                         } catch (Exception e) {
                             content = \"[Error extracting content: \" + e.getMessage() + \"]\";
                         }
                         
                         String messageBody = \"\";
                         
                         if (content instanceof Multipart) {
                             Multipart mp = (Multipart) content;
                             for (int j = 0; j < mp.getCount(); j++) {
                                 BodyPart bp = mp.getBodyPart(j);
                                 if (bp.getContentType().toLowerCase().startsWith(\"text/plain\")) {
                                     messageBody = bp.getContent().toString();
                                     break;
                                 }
                             }
                             
                             // If no text/plain part found, try the first part
                             if (messageBody.isEmpty() && mp.getCount() > 0) {
                                 BodyPart bp = mp.getBodyPart(0);
                                 Object partContent = bp.getContent();
                                 if (partContent != null) {
                                     messageBody = partContent.toString();
                                 }
                             }
                         } else if (content != null) {
                             messageBody = content.toString();
                         }
                         messageData.put(\"body\", messageBody);
                         
                         // Add the data to our list
                         messages.add(messageData);
                         
                         // Mark message for deletion if required
                         if (deletemessages) {
                             msg.setFlag(Flags.Flag.DELETED, true);
                         }
                     } catch (Exception e) {
                         System.err.println(\"Error processing message: \" + e.getMessage());
                         e.printStackTrace();
                     }
                 }
                 
                 // Close folder and store (expunge will actually delete marked messages if deletemessages is true)
                 inbox.close(deletemessages);
                 store.close();
                 
                 return messages;
                 ") 
                 `((hostname ,(->jstring hostname))
                   (username ,(->jstring username))
                   (password ,(->jstring password))
                   (maxmessages ,(if max-messages (->jint max-messages) (->jint -1)))
                   (deletemessages ,(->jboolean delete-messages)))))
             (message-count (->number (j "messages.size();" `((messages ,messages))))))
        
        ;; Process all messages and put them in the queue
        (let loop ((index 0))
          (if (< index message-count)
              (begin
                (let ((message (j "messages.get(index);" `((messages ,messages) (index ,(->jint index))))))
                  (queue 'put-java message))
                (loop (+ index 1)))))
        
        ;; Return the number of messages processed
        message-count)))
  
  ;; Test function for pump-imaps-inbox-to-work-queue
  ;; Creates a queue, fetches one message, and displays it
  (define test-pump-imaps-inbox-to-work-queue
    (lambda* ((imaps-server-hostname: hostname)
              (username: username)
              (password: password))
      ;; Create an empty work queue
      (let ((queue (make-queue)))
        ;; Pump only one message from the inbox to the queue
        (let ((message-count (pump-imaps-inbox-to-work-queue
                              'imaps-server-hostname: hostname
                              'username: username
                              'password: password
                              'work-queue: queue
                              'max-messages: 1)))
          
          (display "Processed ") (display message-count) (display " messages") (newline)
          
          ;; Get the message from the queue and display it
          (let ((message (queue 'poll-java)))
            (if (not (eqv? message 'empty))
                (begin
                  (display "Message found in queue:") (newline)
                  (d/n (beautify-json (scheme->json (->scm-object message)))))
                (begin
                  (display "No messages in inbox") (newline))))))))
)
