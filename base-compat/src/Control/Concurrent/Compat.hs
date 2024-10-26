{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Control.Concurrent.Compat (
  module Base
, forkFinally
, forkOSWithUnmask
) where

import Control.Concurrent as Base
