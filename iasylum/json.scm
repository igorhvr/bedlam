(require-extension (lib iasylum/packrat))
(require-extension (lib iasylum/math))
(require-extension (srfi 69))
(require-extension (srfi 23))

(module iasylum/json
  (json-read json-write scheme->json json->scheme)  
  
  (include "json/json-code.scm")
  
  (define (scheme->json structure)
    (call-with-output-string
     (lambda (output-port)
       (json-write
        structure
        output-port))))
  
  (define (json->scheme string)
    (call-with-input-string
        string
      (lambda (input-port)
        (json-read input-port))))

)
