#!/bin/bash
/base/bedlam/sisc/sisc-1.16.6/sisc -e ' (begin (define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")) "bedlam-enabled sisc started.")' "$@"
