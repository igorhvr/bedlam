(require-extension (lib iasylum/jcode))

(module iasylum/javascript
  (js-manager js-manager-no-optimization run-js/s run-js-no-optimization/s)

  (include "javascript-code.scm"))