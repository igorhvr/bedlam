(require-extension (lib iasylum/javascript))

(module iasylum/local-br
  (validate-cnpj)
  
  (define (validate-cnpj cnpj)
    (->scm-object
     (js
     "function validateCNPJ(cnpj) {

       cnpj = cnpj.replace(/[^\\d]+/g,'');
       if(cnpj == '') return false;
       if (cnpj.length != 14) return false;
       if (cnpj == '00000000000000' || cnpj == '11111111111111' || 
           cnpj == '22222222222222' || cnpj == '33333333333333' || 
           cnpj == '44444444444444' || cnpj == '55555555555555' || 
           cnpj == '66666666666666' || cnpj == '77777777777777' || 
           cnpj == '88888888888888' || cnpj == '99999999999999') {
           return false;
       }
   
       tdsize = cnpj.length - 2
       tnumbers = cnpj.substring(0,tdsize);
       tdigits = cnpj.substring(tdsize);
       tsum = 0;
       pos = tdsize - 7;
       for (i = tdsize; i >= 1; i--) {
         tsum += tnumbers.charAt(tdsize - i) * pos--;
         if (pos < 2) pos = 9;
       }
       calcresult = tsum % 11 < 2 ? 0 : 11 - tsum % 11;
       if (calcresult != tdigits.charAt(0)) return false;
   
       tdsize = tdsize + 1;
       tnumbers = cnpj.substring(0,tdsize);
       tsum = 0;
       pos = tdsize - 7;
       for (i = tdsize; i >= 1; i--) {
         tsum += tnumbers.charAt(tdsize - i) * pos--;
         if (pos < 2) pos = 9;
       }
       calcresult = tsum % 11 < 2 ? 0 : 11 - tsum % 11;
       if (calcresult != tdigits.charAt(1)) return false;

       return true;   
      }; validateCNPJ(mocnpjp);"
     `((mocnpjp ,(->jstring (@iasylum/iasylum::display-string cnpj)))))))
  )
