#!/bin/bash
runhaskell dumptypes.hs Prelude.Compat | typediff check/Prelude.Compat.types -
runhaskell dumptypes.hs Control.Monad.Compat | typediff check/Control.Monad.Compat.types -
runhaskell dumptypes.hs Data.Foldable.Compat | typediff check/Data.Foldable.Compat.types -
runhaskell dumptypes.hs Data.Traversable.Compat | typediff check/Data.Traversable.Compat.types -
runhaskell dumptypes.hs Data.List.Compat | typediff check/Data.List.Compat.types -
