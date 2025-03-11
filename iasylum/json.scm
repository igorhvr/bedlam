(require-extension (lib iasylum/packrat))
(require-extension (lib iasylum/math))
(require-extension (lib iasylum/srfi-89))
(require-extension (srfi 69))
(require-extension (srfi 23))
(require-extension (srfi 19))
(require-library 'iasylum/irregex)

(module iasylum/json
  (json-read json-write scheme->json json->scheme beautify-json json->sxml json->sxml-block sort-json-object-by-keys add-missing-properties-to-json-object header-and-data-list->json ser-deser-test)
  
  (include "json/json-code.scm")  

)
