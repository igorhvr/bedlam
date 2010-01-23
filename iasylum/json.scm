(require-extension (lib iasylum/packrat))
(require-extension (srfi 69))

(module iasylum/json
  (json-read json-write)
  
  (include "json/json-code.scm"))