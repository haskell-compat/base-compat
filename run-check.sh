#!/bin/bash
runhaskell dumptypes.hs Prelude | typediff check/Prelude.types -
runhaskell dumptypes.hs Data.Foldable | typediff check/Data.Foldable.types -
