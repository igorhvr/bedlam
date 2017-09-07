#!/bin/sh

# Compilation prior to running speeds things up *a lot*.
#
# Simple tests without compiled version of bedlam:
#   igorhvr@nbdasylum:~/idm/i$ time /base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (j "System.exit(0);")'
#   
#   real	0m14.645s
#   user	0m20.238s
#   sys	0m3.624s
#   igorhvr@nbdasylum:~/idm/i$ time /base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (j "System.exit(0);")'
#   
#   real	0m15.153s
#   user	0m21.759s
#   sys	0m3.822s
#
# Same tests, now with the compiled version:
#
#   igorhvr@nbdasylum:~/idm/i$ time /base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (j "System.exit(0);")'
#   
#   real	0m3.082s
#   user	0m8.303s
#   sys	0m0.942s
#   igorhvr@nbdasylum:~/idm/i$ time /base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (j "System.exit(0);")'
#   
#   real	0m2.985s
#   user	0m8.393s
#   sys	0m0.731s


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/work-queue.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/work-queue.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/work-queue-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/work-queue-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/debug-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/debug-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/memoize.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/memoize.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/srfi-89.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/srfi-89.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/password-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/password-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/assert.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/assert.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/match-cond-expand.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/match-cond-expand.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/match-slib.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/match-slib.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/matchable_adapted.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/matchable_adapted.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/matchable.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/matchable.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/match-simple.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/match-simple.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/matchable-test.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/chicken/matchable-test.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match-cond-expand.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match-cond-expand.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match-simple.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/other/matchable/synthcode/match-simple.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/match.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/match.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match/match-simple.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match/match-simple.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/read-line.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/read-line.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt-c.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt-c.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/srfi-33.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/srfi-33.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-round.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-round.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/let-optionals.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/let-optionals.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt-js.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/test-fmt-js.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-column.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-column.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-pretty.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-pretty.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-mzscheme.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-mzscheme.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/mantissa.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/mantissa.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-scheme48.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-scheme48.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-mzscheme.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-mzscheme.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-mzscheme.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-mzscheme.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/srfi-69.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/srfi-69.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/make-eq-table.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/make-eq-table.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/string-ports.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/string-ports.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-c-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-mzscheme.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-mzscheme.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-js-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-mzscheme.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-color-mzscheme.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt/fmt-0.8.1/fmt-unicode.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/init.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/init.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/incanter.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/incanter.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/packrat/packrat-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/packrat/packrat-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/packrat/chicken_scheme_files/packrat.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/packrat/chicken_scheme_files/packrat.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/packrat/chicken_scheme_files/doc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/packrat/chicken_scheme_files/doc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/srfi-88.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/srfi-88.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/net.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/net.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/crypto/crypto-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/crypto/crypto-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/sparse/sparse-vectors/sparse-vectors.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/sparse/sparse-vectors/sparse-vectors.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/sparse/sparse-vectors/sparse-vectors-utils.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/sparse/sparse-vectors/sparse-vectors-utils.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/sparse/2d-array/2d-array.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/sparse/2d-array/2d-array.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/jmx.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/jmx.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/jdbc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/jdbc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/scrypt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/scrypt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/i.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/i.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/mosel-code-generator-utils.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/mosel-code-generator-utils.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/gen-class-junk.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/gen-class-junk.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/migration.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/migration.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/m.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/m.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/old-mess/jar-dc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/old-mess/jar-dc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/random.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/random.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/schelog.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/schelog.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/mergesort.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/mergesort.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/clojure-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/clojure-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/math.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/math.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/match.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/match.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/let-optionals/let-optionals.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/let-optionals/let-optionals.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/let-optionals/let-optionals-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/let-optionals/let-optionals-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/hornetq.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/hornetq.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/tagged-begin.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/tagged-begin.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-utils.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-utils.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-all.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-all.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/error.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/error.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-pcre.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-pcre.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-scsh.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-scsh.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/draw-graph.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/draw-graph.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-utf8.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-irregex-utf8.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/benchmarks/run-bench.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/benchmarks/run-bench.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/read-string.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/read-string.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-guile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/irregex-guile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-cset.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.3/test-cset.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/benchmarks/run-bench.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/benchmarks/run-bench.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-cset.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-cset.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-utils.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-utils.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-gauche.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-gauche.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-utf8.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-utf8.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-pcre.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-pcre.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-chicken.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-chicken.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-guile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/irregex-guile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/error.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/error.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/read-string.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/read-string.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-all.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-all.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-scsh.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/test-irregex-scsh.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/draw-graph.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex/irregex-0.9.2/draw-graph.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/javascript-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/javascript-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/iasylum.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/iasylum.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/quartz.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/quartz.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/password.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/password.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/irregex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/irregex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/email.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/email.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/json.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/json.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/init-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/init-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/srfi-89-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/srfi-89-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/crypto.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/crypto.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/log.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/log.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/quartz/quartz-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/quartz/quartz-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/javascript.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/javascript.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/aws.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/aws.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/iasylum-sisc.init.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/iasylum-sisc.init.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/trnscrpt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/trnscrpt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cltime.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cltime.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/color.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/color.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/comparse.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/comparse.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/format.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/format.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/genwrite.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/genwrite.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scamacr.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scamacr.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/collect.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/collect.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaglob.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaglob.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/glob.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/glob.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/psxtime.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/psxtime.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dynamic.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dynamic.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/crc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/crc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dft.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dft.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mkclrnam.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mkclrnam.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/priorque.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/priorque.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/selfset.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/selfset.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/getparam.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/getparam.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synrul.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synrul.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/math-integer.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/math-integer.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwsynrul.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwsynrul.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/record.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/record.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/differ.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/differ.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/solid.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/solid.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/factor.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/factor.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/minimize.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/minimize.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-61.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-61.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/linterp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/linterp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strsrch.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strsrch.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/comlist.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/comlist.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbcom.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbcom.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/chap.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/chap.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/break.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/break.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/charplot.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/charplot.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/fluidlet.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/fluidlet.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ppfile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ppfile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwexpand.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwexpand.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/randinex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/randinex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc2.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc2.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/macrotst.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/macrotst.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/schmooz.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/schmooz.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/repl.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/repl.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/macwork.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/macwork.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/daylight.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/daylight.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tzfile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tzfile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dwindtst.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dwindtst.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strport.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strport.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sierpinski.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sierpinski.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/coerce.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/coerce.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mkpltcat.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mkpltcat.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/collectx.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/collectx.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/require.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/require.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mklibcat.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mklibcat.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mbe.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mbe.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/yasyn.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/yasyn.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/withfile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/withfile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaoutp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaoutp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/queue.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/queue.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/vet.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/vet.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/object.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/object.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synclo.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synclo.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sort.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sort.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/limit.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/limit.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/math-real.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/math-real.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/hashtab.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/hashtab.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/byte.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/byte.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/http-cgi.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/http-cgi.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/bytenumb.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/bytenumb.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/prec.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/prec.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-2.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-2.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/Bev2slib.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/Bev2slib.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbinterp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbinterp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cring.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cring.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/colorspc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/colorspc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/xml-parse.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/xml-parse.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/recobj.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/recobj.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/timecore.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/timecore.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mulapply.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mulapply.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tree.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tree.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/simetrix.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/simetrix.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/formatst.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/formatst.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/peanosfc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/peanosfc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/lineio.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/lineio.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-8.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-8.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/alistab.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/alistab.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-23.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-23.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/pnm.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/pnm.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ratize.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ratize.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwdenote.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mwdenote.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/getopt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/getopt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-1.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-1.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/top-refs.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/top-refs.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/db2html.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/db2html.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbutil.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbutil.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synchk.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/synchk.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/subarray.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/subarray.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/hash.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/hash.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-11.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-11.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/eval.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/eval.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/rdms.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/rdms.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/Template.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/Template.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/clrnamdb.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/clrnamdb.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scainit.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scainit.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/soundex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/soundex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ncbi-dna.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/ncbi-dna.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/defmacex.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/defmacex.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/promise.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/promise.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc4sc3.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc4sc3.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/qp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/qp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/html4each.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/html4each.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cvs.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/cvs.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/modular.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/modular.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/uri.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/uri.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/phil-spc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/phil-spc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/alist.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/alist.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/process.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/process.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/obj2str.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/obj2str.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/manifest.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/manifest.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/logical.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/logical.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/trace.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/trace.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/root.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/root.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dirs.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dirs.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/structure.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/structure.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/null.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/null.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/values.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/values.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc4opt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/sc4opt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/pp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/pp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/random.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/random.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbsyn.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbsyn.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/arraymap.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/arraymap.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/transact.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/transact.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/wttree.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/wttree.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/wttest.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/wttest.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/stdio.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/stdio.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbrowse.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dbrowse.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/printf.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/printf.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scanf.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scanf.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/determ.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/determ.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-9.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi-9.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strcase.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/strcase.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/srfi.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaexpp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scaexpp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dynwind.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/dynwind.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tsort.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/tsort.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/htmlform.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/htmlform.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scmacro.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/scmacro.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/paramlst.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/paramlst.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/debug.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/debug.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/colornam.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/colornam.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/array.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/array.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/matfile.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/matfile.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/batch.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/batch.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mularg.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/mularg.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/grapheps.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/grapheps.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/r4rsyn.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/r4rsyn.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/timezone.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib/3b2/timezone.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/loop.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/loop.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/macros.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/macros.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/datomic.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/datomic.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/i18n.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/i18n.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/random-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/random-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/debug.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/debug.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/date.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/date.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/tmp.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/tmp.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/jcode.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/jcode.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/jcode-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/jcode-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/excel.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/excel.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/fmt.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/fmt.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/slib.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/slib.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/json/json-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/json/json-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/json/chicken_scheme_files/json.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/json/chicken_scheme_files/json.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/json/chicken_scheme_files/doc.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/json/chicken_scheme_files/doc.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/loop/loop.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/loop/loop.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/access.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/access.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/packrat.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/packrat.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/datomic-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/datomic-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/srfi-88-code.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/srfi-88-code.scm")))(j "System.exit(0);")'


/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/clojure.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/clojure.scm")))(j "System.exit(0);")'

/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/csv.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/csv.scm")))(j "System.exit(0);")'

/base/bedlam/sisc/sisc-1.16.6/sisc -e '(define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) (d/n "Will compile" "/home/igorhvr/idm/bedlam/iasylum/bot.scm")(with/fc (lambda p (display p) (j "System.exit(0);")) (lambda () (smart-compile "/home/igorhvr/idm/bedlam/iasylum/bot.scm")))(j "System.exit(0);")'
