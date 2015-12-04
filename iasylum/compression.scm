(require-extension (lib iasylum/jcode))

(module iasylum/compression
  (lzw-compress
   lzw-decompress)

  (include "compression/compression-code.scm"))
