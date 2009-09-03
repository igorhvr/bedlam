;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))

;; Sends an email according to parameters.
;; Depends on:
;;    activation-1.1.jar  commons-email-1.1.jar  mail.jar
;;    iasylum/jcode
(module iasylum/email
  (send-email)
  
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
         (messagetext ,(->jstring messagetext)))))))
  

