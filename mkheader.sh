#!/bin/bash
#
# Generate a module stub, that re-exports the corresponding module from base.
#

set -o nounset
set -o errexit

VERSION=$(ghc --version | sed 's/.*version \(.*\)/\1/')

DIR=includes/ghc-$VERSION
DST=$DIR/base-compat.h

mkdir -p $DIR
cp dist/build/autogen/cabal_macros.h $DST
git add $DST

echo $VERSION
BASE=$(ghc-pkg list | grep base | sed 's/.*base-//')
echo $BASE
cat $DST | grep ' VERSION_base ' | sed 's/.*"\(.*\)"/\1/'

echo " * \`ghc-$VERSION\` / \`base-$BASE\`"
