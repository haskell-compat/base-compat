module TypeDiff (
  typeDiff

-- exported for testing
, sigMap
, typeEq
, alphaNormalize
, normalizeConstrainNames
) where

import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Map as Map (Map)
import qualified Data.Map as Map
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.Parser

import           Data.Generics.Uniplate.Data

typeDiff :: String -> String -> String
typeDiff input1 input2 = unlines (missing ++ extra ++ wrongSigs)
  where
    sigs1 = sigMap input1
    sigs2 = sigMap input2

    names1 = Map.keys sigs1
    names2 = Map.keys sigs2

    missing = map format (names1 \\ names2)
      where
        format :: String -> String
        format = ("missing " ++)

    extra = map format (names2 \\ names1)
      where
        format :: String -> String
        format = ("extra " ++)

    wrongSigs :: [String]
    wrongSigs
      | null mismatches = []
      | otherwise = "wrong types:" : mismatches
      where
        mismatches :: [String]
        mismatches = (catMaybes . map checkType . Map.toList) sigs1

        checkType :: (String, String) -> Maybe String
        checkType (name, t1) = case Map.lookup name sigs2 of
          Just t2 | not (parseType_ t1 `typeEq` parseType_ t2) -> Just (format name t1 ++ "\n" ++ format name t2)
          _ -> Nothing

        format :: String -> String -> String
        format name type_ = "  " ++ name ++ " :: " ++ type_

parseType_ :: String -> Type
parseType_ type_ = case parseType type_ of
  ParseOk t -> t
  _ -> error ("can not parse type " ++ show type_)

sigMap :: String -> Map String String
sigMap = Map.fromList . map splitType . lines
  where
    splitType :: String -> (String, String)
    splitType = fmap stripSigMark . span (not . isSpace)

    stripSigMark :: String -> String
    stripSigMark = dropWhile isSpace . dropWhile (== ':') . dropWhile isSpace

typeEq :: Type -> Type -> Bool
typeEq t1 t2 = normalize t1 == normalize t2
  where normalize = alphaNormalize . normalizeConstrainNames . normalizeConstrains . sortConstrains

sortConstrains :: Type -> Type
sortConstrains x = case x of
  TyForall a1 constrains a2 -> TyForall a1 (sort constrains) a2
  _ -> x

normalizeConstrains :: Type -> Type
normalizeConstrains t = case t of
  TyForall a1 [ParenA a2] a3 -> TyForall a1 [a2] a3
  _ -> t

alphaNormalize :: Type -> Type
alphaNormalize t = transformBi f t
  where
    f :: Name -> Name
    f name = fromMaybe name $ lookup name mapping

    names :: [Name]
    names = (nub . filter isTyVar . universeBi) t

    isTyVar :: Name -> Bool
    isTyVar x = case x of
      Ident n -> null (takeWhile isUpper n)
      _ -> False

    mapping :: [(Name, Name)]
    mapping = zip names vars

    vars :: [Name]
    vars = map (Ident . ('t' :) . show) [0 :: Integer ..]

normalizeConstrainNames :: Type -> Type
normalizeConstrainNames t = transformBi f t
  where
    f :: QName -> QName
    f name = case name of
      Qual (ModuleName "GHC.Base") (Ident "Applicative") -> Qual (ModuleName "Control.Applicative") (Ident "Applicative")
      Qual (ModuleName "GHC.Base") (Ident "Alternative") -> Qual (ModuleName "Control.Applicative") (Ident "Alternative")
      Qual (ModuleName "GHC.Base") (Ident "MonadPlus") -> Qual (ModuleName "Control.Monad") (Ident "MonadPlus")
      Qual (ModuleName "GHC.Base") (Ident "Maybe") -> Qual (ModuleName "Data.Maybe") (Ident "Maybe")
      Qual (ModuleName "GHC.Base") (Ident "Monoid") -> Qual (ModuleName "Data.Monoid") (Ident "Monoid")
      Qual (ModuleName "GHC.Types") (Ident "Bool") -> Qual (ModuleName "GHC.Bool") (Ident "Bool")
      Qual (ModuleName "GHC.Types") (Ident "Ordering") -> Qual (ModuleName "GHC.Ordering") (Ident "Ordering")
      _ -> name
