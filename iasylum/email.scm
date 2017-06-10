;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.
(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/log))

;; Sends an email according to parameters.
;; Depends on:
;;    activation-1.1.jar  commons-email-1.4.jar  mail.jar
;;    iasylum/jcode
(module iasylum/email
  (send-email)


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

      (j 
       "import org.apache.commons.mail.MultiPartEmail;
       email = new MultiPartEmail();
       email.setCharset(\"utf-8\");
       email.setSmtpPort(smtpport);
       email.setSslSmtpPort(sslsmtpport);
       email.setHostName(mailserver);"
       `((mailserver ,(->jstring mailserver))
         (usessl ,(->jboolean use-ssl))
         (smtpport ,(->jint smtp-port))
         (sslsmtpport ,(->jstring (number->string ssl-smtp-port)))
         ))

      (when attachments-file-path
        (for-each
         (lambda (filepath)
           (j "email.attach(new java.io.File(filepath));"
              `((filepath ,(->jstring filepath)))))
         attachments-file-path))

      (when to
        (for-each (match-lambda ((vemail vname)
                                 (j "email.getToAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));" `((lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                                (vemail
                                 (j "email.getToAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));" `((lccemail ,(->jstring vemail))  )))
                                )
                  to))

      (when cc
        (for-each (match-lambda ((vemail vname)
                                 (j "email.getCcAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));" `((lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                                (vemail
                                 (j "email.getCcAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));" `((lccemail ,(->jstring vemail))  )))
                                )
                  cc))

      (when bcc
        (for-each (match-lambda ((vemail vname)
                                 (j "email.getBccAddresses().add(new javax.mail.internet.InternetAddress(lccemail, lccname, \"UTF-8\"));" `((lccemail ,(->jstring vemail)) (lccname ,(->jstring vname)))))
                                (vemail
                                 (j "email.getBccAddresses().add(new javax.mail.internet.InternetAddress(lccemail, true));" `((lccemail ,(->jstring vemail)) )))
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
       `((mailserver ,(->jstring mailserver))
         (subject ,(->jstring subject))
         (messagetext ,(->jstring messagetext))
         (usessl ,(->jboolean use-ssl))
         (usestarttls ,(->jboolean use-starttls))
         (authenticationlogin ,(->jstring authentication-login))
         (authenticationpassword ,(->jstring authentication-password))
         (senderemail ,(->jstring senderemail))
         (sendername ,(->jstring sendername))
         ))))

  ; WIP.
  ; Returns a list containing, for each element for which both fn and raw-fn did not return #f, 3-element-lists where the head is the message id and the next two elements are the results of fn and raw-fn
;  (define receive-imaps-all-inbox-email
;    (lambda* ((imaps-server-hostname: hostname)
;              (username: username)
;              (password: password)
;              
;              ;; fn will receive an alist with message data for each message.
;              ;; The keys are 'subject 'body 'to 'from
;              ;;              'cc 'message-id 'sent-date 'received-date
;              ;;              'charset 'attachment-base64
;              ;;              'spf-status 'dkim-status
;              (fn: fn identity)
;              
;              ;; raw-fn if provided will receive instances of javax.mail.Message
;              (raw-fn: raw-fn #f))
;              
;              (j (quote-convert "//J'
;                 import java.util.*; import javax.mail.*;
;                 props = new Properties();
;                 session = Session.getInstance(props, null);
;                 Store store = session.getStore('imaps');
;                 store.connect(hostname, username, password);
;                 Folder inbox = store.getFolder('INBOX');
;                 inbox.open(Folder.READ_WRITE);
;                 
;                 for (int i = 1; i <= inbox.getMessageCount(); i++) {
;                     Message msg = inbox.getMessage(i);
;
;                     Address[] fromAddresses = msg.getFrom();
;
;                     String[] fromAddressesString = new String[address.length];                     
;                     for (int i = 0 ; i < in.length; i++) {
;                 	fromAddressesString[i] = fromAddresses[i].toString();
;                     }
;                 
;                     subject = msg.getSubject();
;                     sentDate = msg.getSentDate();
;                     content = '';
;                     
;                     if (msg.getContent() instanceof Multipart) {
;                        mp = (Multipart) msg.getContent();
;                        bp = mp.getBodyPart(0);
;                        content = bp.getContent();
;                     } else {
;                        content = msg.getContent();
;                     }
;
;                     System.out.println('Subject: ' + subject);
;                     System.out.println('Content: ' + content);
;                 
;                     msg.setFlag(Flags.Flag.DELETED, true);
;                 } 
;                 inbox.close(true);") `((hostname ,(->jstring hostname))
;                                        (username ,(->jstring username))
;                                        (password ,(->jstring password))
;                                        (fn ,fn)
;                                        (raw-fn)))))
)
