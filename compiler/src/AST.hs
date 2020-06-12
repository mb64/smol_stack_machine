module AST where

import Data.Word

type Ident = String

data Atom = Name Ident
          | Lit Word8
          | LitName Ident
          | Lambda Expr
          deriving (Show, Eq, Ord)

type Expr = [Atom]

data Def = Def Ident Expr
         deriving (Show, Eq, Ord)
