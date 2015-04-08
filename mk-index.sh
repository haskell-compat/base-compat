#!/bin/bash
runhaskell dumpindex.hs Prelude.Compat          | sort > check/Prelude.Compat.index
runhaskell dumpindex.hs Data.Foldable.Compat    | sort > check/Data.Foldable.Compat.index
runhaskell dumpindex.hs Data.Traversable.Compat    | sort > check/Data.Traversable.Compat.index
runhaskell dumpindex.hs Data.List.Compat        | sort > check/Data.List.Compat.index
