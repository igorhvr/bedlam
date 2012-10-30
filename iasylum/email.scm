;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/log))

;; Sends an email according to parameters.
;; Depends on:
;;    activation-1.1.jar  commons-email-1.1.jar  mail.jar
;;    iasylum/jcode
(module iasylum/email
  (send-email send-email-k)
  
  ;;Test: (send-email "localhost" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "assunto" "teste" )

  ;; (require-extension (lib iasylum/jcode))
  (define send-email
    (lambda (mailserver recipientemail recipientname senderemail sendername subject messagetext)
      (log-debug 'send-email mailserver recipientemail recipientname senderemail sendername subject messagetext)
      (j 
       "import org.apache.commons.mail.SimpleEmail;
       SimpleEmail email = new SimpleEmail();
       email.setHostName(mailserver);
       email.addTo(recipientemail, recipientname);
       email.setFrom(senderemail, sendername);
       email.setSubject(subject);
       email.setMsg(messagetext);
       email.send();"
       `((mailserver ,(->jstring mailserver))
         (recipientemail ,(->jstring recipientemail))
         (recipientname ,(->jstring recipientname))
         (senderemail ,(->jstring senderemail))
         (sendername ,(->jstring sendername))
         (subject ,(->jstring subject))
         (messagetext ,(->jstring messagetext))))))

  ;;Test: (send-email "localhost" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "igorhvr@iasylum.net" "assunto" "teste" )


                 
  
  (define send-email-k
    (lambda* ((smtp-server: mailserver "localhost")
         (recipient-email: recipientemail) (recipient-name: recipientname recipientemail)
         (cc-email: ccemail recipientemail) (cc-name: ccname ccemail)
         (to-kv: tokv #f)
         (cc-kv: cckv #f)
         (sender-email: senderemail recipientemail) (sender-name: sendername senderemail)
         (authentication-login: authentication-login "") (authentication-password: authentication-password #f) (use-ssl: use-ssl #f)
         (subject: subject "") (message-text: messagetext "="))
      (log-debug 'send-email mailserver recipientemail recipientname senderemail sendername subject messagetext)

      
      
      (j 
       "import org.apache.commons.mail.SimpleEmail;
       email = new SimpleEmail();
       email.setHostName(mailserver);
       email.addTo(recipientemail, recipientname);
       email.addCc(ccemail, ccname);"
       `((mailserver ,(->jstring mailserver))
         (recipientemail ,(->jstring recipientemail))
         (recipientname ,(->jstring recipientname))
         (ccemail ,(->jstring ccemail))
         (ccname ,(->jstring ccname))
         (senderemail ,(->jstring senderemail))
         (sendername ,(->jstring sendername))
         (subject ,(->jstring subject))
         (messagetext ,(->jstring messagetext))
         (usessl ,(->jboolean use-ssl))
         (authenticationlogin ,(->jstring authentication-login))
         (authenticationpassword ,(->jstring authentication-password))
         ))

      (when tokv
        (for-each (match-lambda ((vemail vname)
                            (j "email.addTo(lccemail, lccname);" `((lccemail ,(->jstring vemail)) (lccname ,(->jstring vname))))))
                  tokv))
      
      (when cckv
        (for-each (match-lambda ((vemail vname)
                            (j "email.addCc(lccemail, lccname);" `((lccemail ,(->jstring vemail)) (lccname ,(->jstring vname))))))
                  cckv))
      
      (j "
       if(!\"\".equals(authenticationlogin)) {
           email.setAuthentication(authenticationlogin, authenticationpassword);
       }
       email.setSSL(usessl); 
       email.setFrom(senderemail, sendername);
       email.setSubject(subject);
       email.setMsg(messagetext);
       email.send();"
       `((mailserver ,(->jstring mailserver))
         (recipientemail ,(->jstring recipientemail))
         (recipientname ,(->jstring recipientname))
         (ccemail ,(->jstring ccemail))
         (ccname ,(->jstring ccname))
         (senderemail ,(->jstring senderemail))
         (sendername ,(->jstring sendername))
         (subject ,(->jstring subject))
         (messagetext ,(->jstring messagetext))
         (usessl ,(->jboolean use-ssl))
         (authenticationlogin ,(->jstring authentication-login))
         (authenticationpassword ,(->jstring authentication-password))
         ))))
    ;; Test (send-email-k 'recipientemail: "igorhvr@iasylum.net") 
)