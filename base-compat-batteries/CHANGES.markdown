## Changes in 0.11.0 [????.??.??]
 - Reexport `MonadFail(fail)` from `Prelude.Compat` and `Control.Monad.Compat`.

   Because `Prelude.Compat.fail` now corresponds to the `fail` from `MonadFail`
   instead of `Monad`, some care is required to implement `Monad.fail` on
   pre-8.8 versions of GHC. The following template is recommended:

   ```haskell
   import Prelude.Compat
   import qualified Control.Monad      as Monad
   import qualified Control.Monad.Fail as Fail

   data Blah a = ...

   instance Functor Blah where ...
   instance Applicative Blah where ...

   instance Monad.Monad Blah where
     (>>=) = ...
   #if !(MIN_VERSION_base(4,13,0))
     fail = Fail.fail
   #endif

   instance Fail.MonadFail Blah where
     fail = ...
   ```

   This approach is also backwards-compatible with previous releases of
   `base-compat-batteries`.

 - This coincides with the `base-compat-???` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-????-????????)
   for more details.

## Changes in 0.10.5 [2018.10.18]
 - This coincides with the `base-compat-0.10.5` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0105-20181018)
   for more details.

## Changes in 0.10.4 [2018.07.03]
 - Add a `SafeHaskellSpec` test which ensures that certain modules (such as
   `Prelude.Compat`) can be imported in the presence of `Safe`.
 - This coincides with the `base-compat-0.10.4` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0104-20180703)
   for more details.

## Changes in 0.10.3 [2018.07.02]
 - This coincides with the `base-compat-0.10.3` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0103-20180702)
   for more details.

## Changes in 0.10.2 [2018.07.02]
 - Sync with `base-4.12`/GHC 8.6
 - Introduce the `Data.Functor.Contravariant.Compat` module, which
   reexports `Data.Functor.Contravariant` from `base` (if using GHC 8.6 or
   later) or the `contravariant` library (if using an earlier version of GHC).
 - This coincides with the `base-compat-0.10.2` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0102-20180702)
   for more details.

## Changes in 0.10.1 [2018.04.10]
 - Add `Data.List.NonEmpty.Compat`.
 - Reexport `(Data.Semigroup.<>)` from `Data.Monoid.Compat`.
 - Tighten lower bounds of compat package dependencies.
 - This coincides with the `base-compat-0.10.1` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0101-20180410)
   for more details.

## Changes in 0.10.0 [2018.04.05]
 - Sync with `base-4.11`/GHC 8.4
 - This coincides with the `base-compat-0.10` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0100-20180405)
   for more details.
