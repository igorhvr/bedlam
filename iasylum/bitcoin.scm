(module iasylum/bitcoin
  (bitcoin/sign bitcoin/valid-signature? test-bitcoin/valid-signature? test-bitcoin/sign)

  ;; Please provide a private Key in WIF Compressed format. 52 characters base58, starts with a 'K' or 'L'.
  ;; Example: KwYB32QP2fhPzUNL1FTRWQMoxFYzHcCXaKXfhsJD4jZmWYH1yYtX
  (define (bitcoin/sign wif-compressed-private-key message)
    (->string (j "org.bitcoinj.core.DumpedPrivateKey.fromBase58(null,wifpk).getKey().signMessage(message);" `((wifpk ,(->jstring wif-compressed-private-key)) (message ,(->jstring message))))))

  (define (test-bitcoin/sign)
    (assert 
     (bitcoin/valid-signature? "1H3HW3h8se5X7D8o2D926ETDiW7Mfaor8r" (bitcoin/sign "KwYB32QP2fhPzUNL1FTRWQMoxFYzHcCXaKXfhsJD4jZmWYH1yYtX" "I am happy that this finally works.") "I am happy that this finally works."))
    (assert
     (not (bitcoin/valid-signature? "1H3HW3h8se5X7D8o2D926ETDiW7Mfaor8r" (bitcoin/sign "KwYB32QP2fhPzUNL1FTRWQMoxFYzHcCXaKXfhsJD4jZmWYH1yYtX" "I am happy that this finally works NO!!!!") "I am happy that this finally works."))))


  (define (bitcoin/valid-signature? bitcoin-address signature message)
    (->boolean
     (j "try{
       return org.bitcoinj.core.LegacyAddress.fromKey(
             org.bitcoinj.core.Address.fromString(null, address).getParameters(),
             org.bitcoinj.core.ECKey.signedMessageToKey(message, signature))
           .toString().equals(address);
    } catch (Exception e) {
        return false;
    }" `((message ,(->jstring message)) (signature ,(->jstring signature)) (address ,(->jstring bitcoin-address))))))

  (define (test-bitcoin/valid-signature?)
    (assert (bitcoin/valid-signature? "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hv0YkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!"))
    (assert (not (bitcoin/valid-signature? "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hvYkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!")))))
