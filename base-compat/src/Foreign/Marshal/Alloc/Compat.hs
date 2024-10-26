{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.Marshal.Alloc.Compat (
  module Base
, calloc
, callocBytes
) where
import Foreign.Marshal.Alloc as Base
