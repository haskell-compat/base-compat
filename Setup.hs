import Distribution.Simple (UserHooks(..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.Simple.Build.Macros (generate)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    postConf = \_ _ _ lbi -> do
      writeFile "base-compat.h" $ generate (localPkgDescr lbi) lbi
  }
