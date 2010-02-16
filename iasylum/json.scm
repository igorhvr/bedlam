(require-extension (lib iasylum/packrat))
(require-extension (srfi 69))
(require-extension (srfi 23))
(module iasylum/json
  (json-read json-write)
  
  (include "json/json-code.scm"))