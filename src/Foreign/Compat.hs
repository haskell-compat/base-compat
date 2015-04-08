{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.Compat (
  module Base
, module Bits
, module Storable
, module Marshal
) where
import Foreign as Base

import Data.Bits.Compat as Bits
import Foreign.Marshal.Compat as Marshal
import Foreign.Storable.Compat as Storable
