{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Debug.Trace.Compat (
  module Base
, traceId
, traceShowId
, traceM
, traceShowM
, traceWith
, traceShowWith
, traceEventWith
) where

import Debug.Trace as Base

#if !(MIN_VERSION_base(4,18,0))
import Prelude.Compat
#endif

#if !(MIN_VERSION_base(4,18,0))
{-|
Like 'trace', but outputs the result of calling a function on the argument.

>>> traceWith fst ("hello","world")
hello
("hello","world")

/Since: 4.18.0.0/
-}
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

{-|
Like 'traceWith', but uses 'show' on the result of the function to convert it to
a 'String'.

>>> traceShowWith length [1,2,3]
3
[1,2,3]

/Since: 4.18.0.0/
-}
traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

-- | Like 'traceEvent', but emits the result of calling a function on its
-- argument.
--
-- /Since: 4.18.0.0/
traceEventWith :: (a -> String) -> a -> a
traceEventWith f a = traceEvent (f a) a
#endif
