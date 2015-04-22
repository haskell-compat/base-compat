#!/bin/bash

set -o errexit

runhaskell dumpindex.hs Prelude.Compat          | sort > check/Prelude.Compat.index
runhaskell dumpindex.hs Control.Monad.Compat    | sort > check/Control.Monad.Compat.index
runhaskell dumpindex.hs Data.Foldable.Compat    | sort > check/Data.Foldable.Compat.index
runhaskell dumpindex.hs Data.List.Compat        | sort > check/Data.List.Compat.index
