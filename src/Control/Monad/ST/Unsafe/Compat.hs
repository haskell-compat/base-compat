{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
, unsafeSTToIO
) where

import Control.Monad.ST (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
