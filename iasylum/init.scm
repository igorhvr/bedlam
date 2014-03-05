;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

; Usage example:
; (begin (define iasylum-bedlam-location "/home/igorhvr/idm/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))

(if (not (getprop 'bedlam-loaded-and-ready-to-use (interaction-environment)))
    (load "init-code.scm"))