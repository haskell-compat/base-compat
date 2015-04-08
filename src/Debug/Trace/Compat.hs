{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Debug.Trace.Compat (
  module Base
, traceId
, traceShowId
, traceM
, traceShowM
) where
import Debug.Trace as Base

#if !MIN_VERSION_base(4,7,0)
import Prelude.Compat

{-|
Like 'trace' but returns the message instead of a third value.

/Since: 4.7.0.0/
-}
traceId :: String -> String
traceId a = trace a a

{-|
Like 'traceShow' but returns the shown value instead of a third value.

/Since: 4.7.0.0/
-}
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a

{-|
Like 'trace' but returning unit in an arbitrary monad. Allows for convenient
use in do-notation. Note that the application of 'trace' is not an action in the
monad, as 'traceIO' is in the 'IO' monad.

> ... = do
>   x <- ...
>   traceM $ "x: " ++ show x
>   y <- ...
>   traceM $ "y: " ++ show y

/Since: 4.7.0.0/
-}
traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   traceMShow $ x
>   y <- ...
>   traceMShow $ x + y

/Since: 4.7.0.0/
-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show
#endif
