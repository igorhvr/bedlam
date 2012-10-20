(stack-trace-on-error #t)
(for-each require-library
 '(sisc/libs/srfi/srfi-1
   sisc/libs/srfi/srfi-2
   sisc/libs/srfi/srfi-9
   sisc/libs/srfi/srfi-11
   sisc/libs/srfi/srfi-13
   sisc/libs/srfi/srfi-19
   sisc/libs/srfi/srfi-27
   lineio))

(putprop 'home '*config-parameters* (normalize-url (current-url) "sisc/"))
(putprop 'slib '*config-parameters* "/usr/share/slib")
(putprop 'sisc.slib '*config-parameters* "/usr/share/slib")

(class-path-extension-append! '("../../lib/pircbot.jar" "../../sisc-pirc.jar"))