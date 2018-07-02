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
