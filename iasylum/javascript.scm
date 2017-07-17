(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/net))

(module iasylum/javascript
  (js-manager create-thread-local-javascript-manager-retriever get-local-javascript-manager
   js js-v8 js-rhino
   nodejs nodejs-global)

  (include "javascript-code.scm"))
