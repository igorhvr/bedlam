(require-extension (lib iasylum/jcode))

(module iasylum/javascript
  (js-manager create-thread-local-javascript-manager-retriever get-local-javascript-manager js)

  (include "javascript-code.scm"))