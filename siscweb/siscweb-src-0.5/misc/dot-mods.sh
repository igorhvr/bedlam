#!/bin/sh

FILES="`find scm opt/*/scm -name '*.scm'`"

MODULES="`echo $FILES |sed -e 's/[^ ]\([A-Za-z-]\+\/\)\+\([A-Za-z-]*\/[A-Za-z-]*\)\.scm/\2/g'`"

MODULE_REGEXP="`echo $MODULES |sed -e 's/ /|/g' |sed -e 's/\//\\\\\//g'`"


echo "digraph G {"
echo "ratio=\"1.3\";size=\"11,8\""

for f in $FILES; do
    for d in `egrep $MODULE_REGEXP $f |grep ' *(import .*) *' |sed -e 's/(import *\(.*\))/\1/'`; do
        echo "\"`echo $f | sed -e 's/.*\/\(.*\/.*\)\.scm/\1/'`\" -> \"$d\";"
    done
done


echo "}"

