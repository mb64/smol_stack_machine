{-# LANGUAGE DataKinds #-}

module IR where

import Data.List
import Text.Read

import qualified AST
import AST (Ident)
import Assembly (Lit(..), Label(..))

data Prim = Send
          | Drop
          | Add
          | Not
          | Call String
          | VCall
          | Lit Lit
          | Swap
          | Dup
          | Rot Int  -- Rot  3 : ( a b c -- b c a )
          | RotB Int -- RotB 3 : ( a b c -- c a b )
          deriving (Show, Eq, Ord)

primName :: String -> Maybe Prim
primName "send" = Just Send
primName "drop" = Just Drop
primName "add" = Just Add
primName "not" = Just Not
primName "call" = Just VCall
primName "swap" = Just Swap
primName "dup" = Just Dup
primName name
    | "rot" `isPrefixOf` name = Rot <$> readMaybe (drop 3 name)
    | "rotb" `isPrefixOf` name = RotB <$> readMaybe (drop 4 name)
    | otherwise = Nothing

data Atom = Prim Prim
          | IfElse Expr Expr
          deriving (Show, Eq, Ord)

type Expr = [Atom]

data Def = Def Ident Expr
         deriving (Show, Eq, Ord)

lowerDef :: AST.Def 'False 'False -> Def
lowerDef (AST.Def n (AST.FunT ins outs) e) = Def n
    $ [Prim $ RotB $ length ins + 1]
    ++ map lowerA e
    ++ [Prim $ Rot $ length outs + 1]
  where lowerA :: AST.Atom 'False -> Atom
        lowerA (AST.Name name) = case primName name of
            Just prim -> Prim prim
            Nothing -> Prim $ Call name
        lowerA (AST.Lit x) = Prim $ Lit $ LitInt x
        lowerA (AST.LitName name) = Prim $ Lit $ LitLbl $ Lbl name
        lowerA (AST.IfElse ie ee) = IfElse (map lowerA ie) (map lowerA ee)
lowerDef (AST.Def _ _ _) = error "internal error: bad type"

-- IR -> Assembly
--
-- Passes:
-- In CFG:
--  - Graph generation: [Def] -> Prog
--  - Peephole opts on Prim
--  - Merge identical segments, incl. simplifying IfC x x
--  - TODO Constant condition: IfC to Always
--  - TCO: Ret that ends with Call -> Always
--  - Simplification: remove empty Always, empty Ret
-- In Layout:
--  - Dead code elimination: remove unreachable segments
--  - TODO Layout: Select a single linear layout
-- In CodeGen:
--  - TODO Linear compiling: [Prim] -> [Instr Lit]
--  - TODO Peephole opts on Instr Lit
--  - TODO Branch glue: output Assembly
