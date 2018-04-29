(module iasylum/bitcoin
  (is-valid-signature test-is-valid-signature)

  (define (is-valid-signature bitcoin-address signature message)
    (->boolean
     (j "try{
       return org.bitcoinj.core.LegacyAddress.fromKey(
             org.bitcoinj.core.Address.fromString(null, address).getParameters(),
             org.bitcoinj.core.ECKey.signedMessageToKey(message, signature))
           .toString().equals(address);
    } catch (Exception e) {
        return false;
    }" `((message ,(->jstring message)) (signature ,(->jstring signature)) (address ,(->jstring bitcoin-address))))))

  (define (test-is-valid-signature)
    (assert (is-valid-signature "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hv0YkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!"))
    (assert (not (is-valid-signature "1MjFv4bNHjDnD8FdyAgvrEEDaVEpfvyi7h" "H0vLPZxD8Yc9hvYkobbzhQCiNxvjglaHR/8LDqOLr3TOkJveN094R6EBWsnD5BKx5czAjJrgJqUNiiChNq8Sgk=" "Quero trabalhar na vanguarda da economia digital!")))))
