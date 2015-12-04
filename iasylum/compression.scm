(require-extension (lib iasylum/jcode))

(module iasylum/compression
  (lzw-compress
   lzw-uncompress)

  (include "compression/compression-code.scm"))
