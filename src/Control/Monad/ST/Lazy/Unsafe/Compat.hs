{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Lazy.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
) where

import Control.Monad.ST.Lazy (unsafeInterleaveST, unsafeIOToST)
