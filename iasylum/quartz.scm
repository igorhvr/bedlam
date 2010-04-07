;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))

(module iasylum/quartz
  (create-scheduler create-quartz-job-from-closure create-quartz-cron-trigger schedule-job)
  (include "quartz/quartz-code.scm"))
