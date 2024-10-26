{-# LANGUAGE NoImplicitPrelude #-}
module Data.Monoid.Compat (
  module Base
, (<>)
) where

import Data.Monoid as Base hiding ((<>))
import Data.Semigroup ((<>))
