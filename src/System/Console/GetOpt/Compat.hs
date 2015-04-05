#if !MIN_VERSION_base(4,7,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module System.Console.GetOpt.Compat (
  module Base
) where
import System.Console.GetOpt as Base

#if !MIN_VERSION_base(4,7,0)
instance Functor ArgOrder where
    fmap _ RequireOrder      = RequireOrder
    fmap _ Permute           = Permute
    fmap f (ReturnInOrder g) = ReturnInOrder (f . g)

instance Functor OptDescr where
    fmap f (Option a b argDescr c) = Option a b (fmap f argDescr) c

instance Functor ArgDescr where
    fmap f (NoArg a)    = NoArg (f a)
    fmap f (ReqArg g s) = ReqArg (f . g) s
    fmap f (OptArg g s) = OptArg (f . g) s
#endif