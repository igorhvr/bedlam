;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/match))
(require-extension (lib iasylum/jcode))
(require-extension (lib iasylum/log))
(require-extension (lib iasylum/iasylum))

(module iasylum/work-queue
  (make-queue
   process-all-work
   get-next-worker-n
   start-worker)
  (import s2j)
  (include "work-queue-code.scm"))



