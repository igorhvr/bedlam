#!/bin/bash
SCHEMEENV=`basename $0`
SOURCEFILE=$1

/base/bedlam/sisc/sisc-1.16.6/sisc -x -e "(srfi-22-prepare '$SCHEMEENV \"$SOURCEFILE\")" -c main-hook -- "$@"

