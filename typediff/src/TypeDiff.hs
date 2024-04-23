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
import qualified Data.List as List
import           Data.Map as Map (Map)
import qualified Data.Map as Map
import           Language.Haskell.Exts.Simple.Extension
import           Language.Haskell.Exts.Simple.Syntax
import           Language.Haskell.Exts.Simple.Parser

import           Data.Generics.Uniplate.Data

typeDiff :: String -> String -> String
typeDiff input1 input2 = unlines (missing ++ extra ++ wrongSigs)
  where
    sigs1 = sigMap input1
    sigs2 = sigMap input2

    names1 = Map.keys sigs1
    names2 = Map.keys sigs2

    missing = map format (names1 List.\\ names2)
      where
        format :: String -> String
        format = ("missing " ++)

    extra = map format (names2 List.\\ names1)
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
parseType_ type_ =
  case parseTypeWithMode
         (defaultParseMode
           { extensions = EnableExtension FlexibleContexts
                        : extensions defaultParseMode
           })
         type_ of
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

normalize :: Type -> Type
normalize = alphaNormalize
          . sortConstrains
          . normalizeConstrainNames
          . normalizeConstrains
          . stripCallStacksFromType

-- GHC 9.2+ are more likely to display to display HasCallStack constraints in
-- :type output. For compatibility with older GHCs, which do not do this, we
-- remove HasCallStack constraints entirely.
stripCallStacksFromType :: Type -> Type
stripCallStacksFromType x = case x of
  TyForall a1 constrains a2 -> TyForall a1 (fmap stripCallStacksFromContext constrains) a2
  _ -> x

stripCallStacksFromContext :: Context -> Context
stripCallStacksFromContext x = case x of
  CxSingle asst | isAsstCallStack asst
                -> CxEmpty
  CxTuple assts -> CxTuple (filter (not . isAsstCallStack) assts)
  _             -> x

isAsstCallStack :: Asst -> Bool
isAsstCallStack (TypeA t) = isTypeCallStack t
isAsstCallStack IParam{}  = False
  -- It's possible that the HasCallStack type synonym could be expanded to its
  -- underlying implicit parameter, but let's wait for that to happen in
  -- practice before worrying about it.
isAsstCallStack (ParenA asst) = isAsstCallStack asst
isAsstCallStack _             = False

isTypeCallStack :: Type -> Bool
isTypeCallStack (TyCon name) = name == Qual (ModuleName "GHC.Stack.Types")          (Ident "HasCallStack") ||
                               name == Qual (ModuleName "GHC.Internal.Stack.Types") (Ident "HasCallStack")
isTypeCallStack _            = False

sortConstrains :: Type -> Type
sortConstrains x = case x of
  TyForall a1 constrains a2 -> TyForall a1 (fmap sortContext constrains) a2
  _ -> x

sortContext :: Context -> Context
sortContext x = case x of
  CxTuple assts -> CxTuple (List.sort assts)
  _             -> x

normalizeConstrains :: Type -> Type
normalizeConstrains t = case t of
  TyForall a1 a2 a3
    |  isNothingOrPred null a1, isNothingOrPred nullContext a2
    -> a3
    |  otherwise
    -> TyForall a1 (fmap normalizeContext a2) a3
  _ -> t
  where
    isNothingOrPred :: (a -> Bool) -> Maybe a -> Bool
    isNothingOrPred _ Nothing  = True
    isNothingOrPred f (Just x) = f x

    nullContext :: Context -> Bool
    nullContext CxEmpty = True
      -- It's possible that there could be a (CxTuple []), but let's wait for
      -- that to happen in practice before worrying about it.
    nullContext _       = False

normalizeContext :: Context -> Context
normalizeContext x = case x of
  CxSingle (ParenA a) -> CxSingle a
  _                   -> x

alphaNormalize :: Type -> Type
alphaNormalize t = transformBi f t
  where
    f :: Name -> Name
    f name = fromMaybe name $ lookup name mapping

    names :: [Name]
    names = (List.nub . filter isTyVar . universeBi) t

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
      Qual (ModuleName "GHC.Base") (Ident "Semigroup") -> Qual (ModuleName "Data.Semigroup") (Ident "Semigroup")
      Qual (ModuleName "GHC.Maybe") (Ident "Maybe") -> Qual (ModuleName "Data.Maybe") (Ident "Maybe")
      Qual (ModuleName "GHC.Types") (Ident "Bool") -> Qual (ModuleName "GHC.Bool") (Ident "Bool")
      Qual (ModuleName "GHC.Types") (Ident "Ordering") -> Qual (ModuleName "GHC.Ordering") (Ident "Ordering")

      Qual (ModuleName "GHC.Internal.Base") (Ident "Applicative") -> Qual (ModuleName "Control.Applicative") (Ident "Applicative")
      Qual (ModuleName "GHC.Internal.Base") (Ident "Alternative") -> Qual (ModuleName "Control.Applicative") (Ident "Alternative")
      Qual (ModuleName "GHC.Internal.Base") (Ident "MonadPlus") -> Qual (ModuleName "Control.Monad") (Ident "MonadPlus")
      Qual (ModuleName "GHC.Internal.Base") (Ident "Monoid") -> Qual (ModuleName "Data.Monoid") (Ident "Monoid")
      Qual (ModuleName "GHC.Internal.Base") (Ident "Semigroup") -> Qual (ModuleName "Data.Semigroup") (Ident "Semigroup")
      Qual (ModuleName "GHC.Internal.Base") i -> Qual (ModuleName "GHC.Base") i
      Qual (ModuleName "GHC.Internal.Data.Foldable") i -> Qual (ModuleName "Data.Foldable") i
      Qual (ModuleName "GHC.Internal.Data.Traversable") i -> Qual (ModuleName "Data.Traversable") i
      Qual (ModuleName "GHC.Internal.Float") i -> Qual (ModuleName "GHC.Float") i
      Qual (ModuleName "GHC.Internal.Maybe") (Ident "Maybe") -> Qual (ModuleName "Data.Maybe") (Ident "Maybe")
      Qual (ModuleName "GHC.Internal.Maybe") i -> Qual (ModuleName "GHC.Maybe") i
      Qual (ModuleName "GHC.Internal.Num") i -> Qual (ModuleName "GHC.Num") i
      Qual (ModuleName "GHC.Internal.Real") i -> Qual (ModuleName "GHC.Real") i

      UnQual (Ident "Foldable") -> Qual (ModuleName "Data.Foldable") (Ident "Foldable")
      UnQual (Ident "MonadFail") -> Qual (ModuleName "Control.Monad.Fail") (Ident "MonadFail")
      UnQual (Ident "Traversable") -> Qual (ModuleName "Data.Traversable") (Ident "Traversable")
      _ -> name
