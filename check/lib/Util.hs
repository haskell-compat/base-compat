{-# LANGUAGE CPP #-}
module Util (inCabalNewRepl, normalizeLines, normalizeSignatures) where

import Data.Char (isAlphaNum, isDigit, isUpper)
import System.Environment (getEnvironment)
import System.Process
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

inCabalNewRepl :: String -> String -> IO String
inCabalNewRepl module_ input_ = do
    -- if HC variable is set, we use it to invoke 'cabal new-repl'
    envvars <- getEnvironment
    let mhc = lookup "HC" envvars
    let withCompiler = maybe [] (\hc -> ["-w", hc]) mhc

    putStrLn "=== INPUT ==="
    putStrLn input
    output <- readProcess "cabal" (args withCompiler) input
    putStrLn "=== OUTPUT ==="
    putStrLn output

    return output
  where
    args withCompiler =
        [ "new-repl"
        , "for-repl"
        , "--ghc-options=\"-ignore-dot-ghci -package-env=-\""
#if __GLASGOW_HASKELL__ == 710
          -- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/123
        , "--ghc-option=-optl-no-pie"
#endif
        , "--enable-tests"
        , "--enable-benchmarks"
        ] ++ withCompiler ++
        [ "--"
        ]

    input = unlines
        -- load module
        [ ":set prompt \"\""
        , ":l " ++ module_
        , show randomString1
        ]
        ++ input_ ++
        unlines
            [ "", show randomString2
            ]

-- These "random" string are markers, which we output around "payload" REPL.
-- As these "random" strings are unlikely to appear in the actual output
-- we can be sure we extract the relevant part of the output.
--
randomString1 :: String
randomString1 = "XHyIGBiBHqPo7PdVNIJV2yAwh60czbwFVKM8FhCL"

randomString2 :: String
randomString2 = "bo1vzYNeOweLxLi2zlLjbHKjXT8nClvdKKSQaehs"

normalizeLines :: String -> String
normalizeLines
    = unlines
    . joinIndentedLines Nothing
    . lines
  where
    joinIndentedLines :: Maybe String ->  [String] -> [String]
    joinIndentedLines Nothing  []  = []
    joinIndentedLines (Just l) [] = [l]
    joinIndentedLines Nothing  (l : ls) = joinIndentedLines (Just l) ls
    joinIndentedLines (Just p) ((' ':' ':l) : ls) =
        joinIndentedLines (Just $ p ++ ' ' : l) ls
    joinIndentedLines (Just p) (l : ls) =
        p : joinIndentedLines (Just l) ls

normalizeSignatures :: String -> String
normalizeSignatures
    = unlines
    . map stripModuleIdentifiers   -- strip package-prefixes from types
    . takeWhile (/= show randomString2)
    . drop 1
    . dropWhile (/= show randomString1)
    . joinIndentedLines Nothing
    . lines
  where
    --
    -- @
    -- foo
    --    :: a
    --    -> b
    -- @
    --
    -- into
    --
    -- @
    -- foo :: a -> b
    -- @
    joinIndentedLines :: Maybe String ->  [String] -> [String]
    joinIndentedLines Nothing  []  = []
    joinIndentedLines (Just l) [] = [l]
    joinIndentedLines Nothing  (l : ls) = joinIndentedLines (Just l) ls
    joinIndentedLines (Just p) ((' ':' ':l) : ls) =
        joinIndentedLines (Just $ p ++ ' ' : l) ls
    joinIndentedLines (Just p) (l : ls) =
        p : joinIndentedLines (Just l) ls

    -- regex is better than manual parsing
    --
    -- replace
    --
    -- @semigroup-version@tag:C@ with @C"
    --
    -- so `typediff` can parse the types
    --
    stripModuleIdentifiers = replace parser
      where
        parser = do
            -- packages: we hardcode the list of packages,
            -- as it's simpler. Adding more (contravariant, bifunctors...)
            -- is easy when needed; and we can be sure that we don't match
            -- something that we shouldn't.
            --
            _ <- P.choice
                [ P.try (P.string "fail")
                , P.try (P.string "ghc-prim")
                , P.try (P.string "semigroups")
                ]

            -- version suffix
            P.optional (P.char '-' >> P.many1 (P.satisfy $ \c -> isDigit c || c == '.'))

            -- e.g. semigroups-0.18.5@73kDpWI4r7OHmGD2br1vF8

            P.optional (P.char '@' >> P.many1 (P.satisfy isAlphaNum))

            -- separator
            _ <- P.char ':'

            -- Upper-case letter: Type-constructor
            c <- P.satisfy isUpper
            cs <- P.getInput

            return (c, cs)

replace :: P.Parser (Char, String) -> String -> String
replace p = go where
    go [] = []
    go str@(c : cs) = case P.runParser p () "<input>" str of
        Left _err       -> c  : go cs
        Right (c', cs') -> c' : go cs'
