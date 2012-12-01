#!/bin/bash
#
# Generate a module stub, that re-exports the corresponding module from base.
#

set -o nounset
set -o errexit

MODULE=$1
FILE=src/$(echo $MODULE | sed 's/\./\//g').hs
DIR=$(echo $FILE | sed 's/\w*.hs$//')
mkdir -p $DIR
echo "module $MODULE (" > $FILE
echo "  module Base" >> $FILE
echo ") where" >> $FILE
echo "import \"base\" $MODULE as Base" >> $FILE
git add $FILE
