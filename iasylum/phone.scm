(require-extension (lib iasylum/jcode))

(module iasylum/phone
  (parse-phone-number)

  ;; Sample usage (to retrieve area-code only):
  ;; (match-let ([((_ . country-code) (_ . area-code) (_ . phone-number)) (parse-phone-number 'string-to-parse: "+5519999999999")]) area-code)
  ;; => "19"
  (define* (parse-phone-number (string-to-parse: string-to-parse)
                               (default-country: default-country "BR")
                               (result-when-invalid: result-when-invalid #f))
    (try-and-if-it-fails-object (result-when-invalid)
     (match-let ([(s1 country-code s3 area-code s5 phone-number)
      (map-jarray (lambda (v) (->scm-object v))
       (j "Object phoneNumberUtil=com.google.i18n.phonenumbers.PhoneNumberUtil.getInstance(); // PhoneNumberUtil
        Object number = phoneNumberUtil.parseAndKeepRawInput(numberstr, defaultcountry); // PhoneNumber
        isNumberValid = phoneNumberUtil.isValidNumber(number);
        if(!isNumberValid) throw new RuntimeException(\"Invalid phone number provided: \"+numberstr);
        Object countryCode = Integer.toString(number.getCountryCode());
        Object areaCodeLength=phoneNumberUtil.getLengthOfGeographicalAreaCode(number);
        Object nationalNumber=Long.toString(number.getNationalNumber());
        Object areaCode=nationalNumber.substring(0,areaCodeLength);
        Object number=nationalNumber.substring(areaCodeLength,nationalNumber.length());
        return new Object[]{\"COUNTRY_CODE\", countryCode, \"AREA_CODE\", areaCode, \"PHONE_NUMBER\", number};"
          `((numberstr ,(->jstring string-to-parse))
            (defaultcountry ,(->jstring default-country)))))))
                `((country-code . ,country-code) (area-code . ,area-code) (phone-number . ,phone-number)))))
  )
