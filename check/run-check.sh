#!/bin/bash

if [ ! -f typediff ]; then
    wget https://github.com/haskell-compat/base-compat/releases/download/typediff-0.1.0/typediff
    chmod +x typediff
fi
PATH=./:$PATH cabal test
