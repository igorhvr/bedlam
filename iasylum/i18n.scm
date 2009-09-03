;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(module iasylum/i18n
  (br-bundle get-message get-messages-bundle)

  ;; (require-extension (lib iasylum/jcode))
  
  (define (br-bundle)
    (get-messages-bundle "pt" "BR" "resources"))
  
  (define get-message
    (case-lambda
      ((bundle key) (->string (j "messages.getString(key);" `((messages ,bundle)   (key ,(->jstring key)))) ))
      ((key) (get-message (br-bundle) key))))
  
  (define (get-messages-bundle language country bundle)
    (j "
            Locale currentLocale = new Locale(language, country);
            ResourceBundle messages = ResourceBundle.getBundle(bundle, currentLocale);
            messages;"
       `((language ,(->jstring language))
         (country ,(->jstring country))
         (bundle ,(->jstring bundle))))))
