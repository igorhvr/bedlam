(require-extension (lib iasylum/packrat))
(require-extension (lib iasylum/math))
(require-extension (srfi 69))
(require-extension (srfi 23))

(module iasylum/json
  (json-read json-write scheme->json json->scheme)  
  
  (include "json/json-code.scm")  

)
