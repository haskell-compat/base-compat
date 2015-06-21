{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Concurrent.Compat (
  module Base
, forkFinally
) where

import Control.Concurrent as Base

#if !(MIN_VERSION_base(4,6,0))
import Control.Exception
import Prelude

-- | fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
--
-- > forkFinally action and_then =
-- >   mask $ \restore ->
-- >     forkIO $ try (restore action) >>= and_then
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
--
-- /Since: 4.6.0.0/
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
