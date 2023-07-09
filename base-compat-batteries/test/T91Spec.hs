{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | A regression test for https://github.com/haskell-compat/base-compat/issues/91.
module T91Spec (main, spec) where

import Data.Tuple.Compat
import Test.Hspec (Expectation, Spec, describe, hspec, it)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Solo data constructor" $
    it "is always available" $ do
      Solo () `seq` return () :: Expectation
