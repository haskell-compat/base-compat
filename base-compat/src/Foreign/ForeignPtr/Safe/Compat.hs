{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.ForeignPtr.Safe.Compat (
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
#if defined(__HUGS__) || defined(__GLASGOW_HASKELL__)
        , FinalizerEnvPtr
#endif
        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
#if defined(__HUGS__) || defined(__GLASGOW_HASKELL__)
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
#endif
        , withForeignPtr

#ifdef __GLASGOW_HASKELL__
        , finalizeForeignPtr
#endif

        -- ** Low-level operations
        , touchForeignPtr
        , castForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
    ) where

import Foreign.ForeignPtr
