{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Compat (
  module Base
, Monad
, MonadFail
, fail
, MonadPlus(..)
) where

import Control.Monad as Base hiding (fail)
import Control.Monad.Fail as Base
