#!/bin/bash

set -o errexit

runhaskell dumpindex.hs Prelude.Compat       | sort > index/Prelude.Compat.index
runhaskell dumpindex.hs Control.Monad.Compat | sort > index/Control.Monad.Compat.index
runhaskell dumpindex.hs Data.Complex.Compat  | sort > index/Data.Complex.Compat.index
runhaskell dumpindex.hs Data.Foldable.Compat | sort > index/Data.Foldable.Compat.index
runhaskell dumpindex.hs Data.List.Compat     | sort > index/Data.List.Compat.index
