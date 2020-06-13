{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST where

-- AST is parametric over:
--  - Macro expansion: 'True if there are macros
--  - Lambda lifting: 'True if there are lambdas
--
-- Macro expansion happens first, then lambdas are lifted into top-level
-- functions.
--
-- Typedefs are treated as macros.

import Control.Monad.RWS
import Data.Foldable
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Word

type Ident = String

data Atom (l :: Bool) where
    Name :: Ident -> Atom l
    Lit :: Word8 -> Atom l
    LitName :: Ident -> Atom l
    Lambda :: Type -> Expr 'True -> Atom 'True

deriving instance Show (Atom l)
deriving instance Eq (Atom l)
deriving instance Ord (Atom l)

type Expr l = [Atom l]

data Type = NameT Ident
          | FunT [Type] [Type]
          deriving (Show, Eq, Ord)

data Def (m :: Bool) (l :: Bool) where
    Def :: Ident -> Type -> Expr l -> Def m l
    TypeDef :: Ident -> Type -> Def 'True l
    Macro :: Ident -> Expr l -> Def 'True l

deriving instance Show (Def m l)
deriving instance Eq (Def m l)
deriving instance Ord (Def m l)

-- FIXME: this relies on the macros being non-recursive, so that Haskell's
-- laziness can do all the work, and doesn't check this precondition.
--
-- If you try to make recursive macros, it will get into an infinite loop.
expandMacros :: forall l. [Def 'True l] -> [Def 'False l]
expandMacros defs = mapMaybe expandDef defs
  where typedefs = Map.fromListWithKey typeConflict $ mapMaybe getTypedef defs
        macros = Map.fromListWithKey macroConflict $ mapMaybe getMacro defs

        typeConflict k _ _ = error $ "multiple definition of type " ++ k
        macroConflict k _ _ = error $ "multiple definition of macro " ++ k

        getTypedef :: Def 'True l -> Maybe (Ident, Type)
        getTypedef (TypeDef n t) = Just (n, expandType t)
        getTypedef _ = Nothing

        expandType :: Type -> Type
        expandType (NameT n) = case Map.lookup n typedefs of
            Just t -> t
            Nothing -> NameT (verifyTypeName n)
        expandType (FunT as bs) = map expandType as `FunT` map expandType bs

        verifyTypeName :: Ident -> Ident
        verifyTypeName "int" = "int"
        verifyTypeName t = error $ "unrecognised type " ++ t

        getMacro :: Def 'True l -> Maybe (Ident, Expr l)
        getMacro (Macro n e) = Just (n, expandExpr e)
        getMacro _ = Nothing

        expandAtom :: Atom l -> Expr l
        expandAtom (Name n) = Map.findWithDefault [Name n] n macros
        expandAtom (Lit x) = [Lit x]
        expandAtom (LitName n)
            | n `Map.member` macros = error $ "Cannot take address of macro " ++ n
            | otherwise = [LitName n]
        expandAtom (Lambda t e) = [Lambda t $ expandExpr e]
        expandExpr = concatMap expandAtom

        expandDef :: Def 'True l -> Maybe (Def 'False l)
        expandDef (Def n t e)
            | n `Map.member` macros = error $ "multiple definition of term " ++ n
            | otherwise = Just $ Def n (expandType t) (expandExpr e)
        expandDef _ = Nothing

liftLambdas :: [Def 'False 'True] -> [Def 'False 'False]
liftLambdas defs = snd $ execRWS (traverse_ go defs) () 0
  where go :: Def 'False 'True -> RWS () [Def 'False 'False] Int ()
        go (Def n t e) = do
            e' <- traverse goA e
            tell [Def n t e']

        genName :: RWS r [w] Int Ident
        genName = do
            n <- get
            put $ n + 1
            pure $ "<lambda #" ++ show n ++ ">"

        goA :: Atom 'True -> RWS () [Def 'False 'False] Int (Atom 'False)
        goA (Name n) = pure $ Name n
        goA (Lit x) = pure $ Lit x
        goA (LitName n) = pure $ LitName n
        goA (Lambda t e) = do
            n <- genName
            go (Def n t e)
            pure $ LitName n

simplifyAst :: [Def 'True 'True] -> [Def 'False 'False]
simplifyAst = liftLambdas . expandMacros
