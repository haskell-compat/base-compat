## Changes in 0.14.1 [2024.12.06]
 - This coincides with the `base-compat-batteries-0.14.1` release. Refer to the
   [`base-compat-batteries` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat-batteries/CHANGES.markdown#changes-in-0141-20241206)
   for more details.
 - Drop support for pre-8.0 versions of GHC.

## Changes in 0.14.0 [2024.04.30]
 - This coincides with the `base-compat-batteries-0.14.0` release. Refer to the
   [`base-compat-batteries` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat-batteries/CHANGES.markdown#changes-in-0140-20240430)
   for more details.

## Changes in 0.13.1 [2023.10.11]
 - This coincides with the `base-compat-batteries-0.13.1` release. Refer to the
   [`base-compat-batteries` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat-batteries/CHANGES.markdown#changes-in-0131-20231011)
   for more details.

## Changes in 0.13.0 [2023.03.10]
 - This coincides with the `base-compat-0.13.0` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0130-20230310)
   for more details.
 - Require `OneTuple-0.4` or later on GHC 7.4+, as that is the first `OneTuple`
   release to backport the `MkSolo` data constuctor for `Solo`. See
   `Data.Tuple.Compat`.
 - Introduce `Data.Foldable1.Compat` and `Data.Bifoldable1.Compat` modules,
   which correspond to changes made in `base-4.18.0.0`. `base-compat-batteries`
   uses the `foldable1-classes-compat` library to backport this code to older
   versions of `base`.
 - Depend on `bifunctor-classes-compat` to backport the `Bifunctor`,
   `Bifoldable`, and `Bitraversable` classes instead of the
   `bifunctors` library, which has more dependencies.

## Changes in 0.12.3 [2023.07.12]
 - Allow building with `OneTuple-0.4.*`.

## Changes in 0.12.2 [2022.08.11]
 - This coincides with the `base-compat-0.12.2` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0122-20220811)
   for more details.

## Changes in 0.12.1 [2021.10.30]
 - Backport `Solo` to the `Data.Tuple.Compat` module. If using `ghc-prim-0.7.0`
   or later, `Solo` is taken from `ghc-prim`. If using an older version of
   `ghc-prim`, `Solo` is taken from the `OneTuple` compatibility library.

 - This coincides with the `base-compat-0.12.1` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0121-20211030)
   for more details.

## Changes in 0.12.0 [2021.08.29]
 - `Data.Semigroup.Compat{.Repl.Batteries}` no longer re-exports the `Option`
   data type or the `option` function, as both have been removed in
   `base-4.16`.

 - This coincides with the `base-compat-0.12.0` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0120-20210829)
   for more details.

## Changes in 0.11.2 [2020.09.30]
 - This coincides with the `base-compat-0.11.2` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0112-20200930)
   for more details.

## Changes in 0.11.1 [2020.01.27]
 - This coincides with the `base-compat-0.11.1` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0111-20200127)
   for more details.

## Changes in 0.11.0 [2019.09.06]
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

 - Introduce the `Data.Type.Equality.Compat` module, which
   reexports `Data.Type.Equality` from `base` (if using a sufficiently recent
   version of GHC) or the `type-equality` library (if using an old GHC).

 - This coincides with the `base-compat-0.11.0` release. Refer to the
   [`base-compat` changelog](https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown#changes-in-0110-20190906)
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
