{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.ST.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
, unsafeSTToIO
) where

import Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
