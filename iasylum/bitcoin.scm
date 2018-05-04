(module iasylum/bitcoin
  (bitcoin/valid-signature? test-bitcoin/valid-signature?)

  (define (bitcoin/valid-signature? bitcoin-address signature message)
    (->boolean
     (j "try{
       return org.bitcoinj.core.LegacyAddress.fromKey(
             org.bitcoinj.core.Address.fromString(null, address).getParameters(),
             org.bitcoinj.core.ECKey.signedMessageToKey(message, signature))
           .toString().equals(address);
    } catch (Exception e) {
        return false;
    }" `((message ,(->jstring message)) (signature ,(->jstring bitcoin-signature)) (address ,(->jstring bitcoin-address))))))

  (define (test-bitcoin/valid-signature?)
    (assert (bitcoin/valid-signature? "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hv0YkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!"))
    (assert (not (bitcoin/valid-signature? "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hvYkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!")))))
