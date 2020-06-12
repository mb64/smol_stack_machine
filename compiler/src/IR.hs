{-# LANGUAGE DataKinds #-}

module IR where

import Control.Monad.Writer
import Data.Foldable
import Data.List
import Data.Word
import Text.Read

import qualified AST
import AST (Ident)

data Prim = Send
          | Drop
          | Add
          | Not
          | Call
          | Ret
          | Swap
          | Dup
          | Rot Int  -- Rot  3 : ( a b c -- b c a )
          | RotB Int -- RotB 3 : ( a b c -- c a b )
          | Nop
          | If
          | IfElse
          deriving (Show, Eq, Ord)

primName :: String -> Maybe Prim
primName "send" = Just Send
primName "drop" = Just Drop
primName "add" = Just Add
primName "not" = Just Not
primName "call" = Just Call
primName "swap" = Just Swap
primName "dup" = Just Dup
primName "nop" = Just Nop
primName "if" = Just If
primName "ifelse" = Just IfElse
primName name
    | "rot" `isPrefixOf` name = Rot <$> readMaybe (drop 3 name)
    | "rotb" `isPrefixOf` name = RotB <$> readMaybe (drop 4 name)
    | otherwise = Nothing

data Atom = Lit Word8
          | LitName Ident
          | Prim Prim
          deriving (Show, Eq, Ord)

type Expr = [Atom]

data Def = Def Ident Expr
         deriving (Show, Eq, Ord)

lowerDef :: AST.Def 'False 'False -> Def
lowerDef (AST.Def n (AST.FunT ins outs) e) = Def n $ execWriter $ do
    tell [Prim $ RotB $ length ins]
    traverse_ (tell . lowerA) e
    tell [Prim $ Rot $ length outs]
    tell [Prim $ Ret]
  where lowerA :: AST.Atom 'False -> Expr
        lowerA (AST.Name name) = case primName name of
            Just prim -> [Prim prim]
            Nothing -> [LitName name, Prim Call]
        lowerA (AST.Lit x) = [Lit x]
        lowerA (AST.LitName name) = [LitName name]
lowerDef (AST.Def _ _ _) = error "internal error: bad type"
