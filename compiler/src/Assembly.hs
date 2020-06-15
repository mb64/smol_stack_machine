module Assembly where

import Data.Bits
import Data.Char
import Data.Word

data Lit = LitInt Word8
         | LitLbl Label
         | LitNot Lit
         | LitAdd Lit Lit
         -- | LitArith Label (Word8 -> Lit)
         deriving (Show, Eq, Ord)

litNot :: Lit -> Lit
litNot (LitInt x) = LitInt $ complement x
litNot l = LitNot l

litAdd :: Lit -> Lit -> Lit
litAdd (LitInt x) (LitInt y) = LitInt $ x + y
litAdd lx ly = LitAdd lx ly

data Instr a = Send
             | Jmp
             | Skip
             | Drop
             | Add
             | Not
             | Swap
             | Dup
             | Resurrect
             | Imm a
             | Nop
             deriving (Show, Eq, Ord)

type Label = String

type Assembly = [Either Label (Instr Lit)]

prettyPrintAsm :: Assembly -> String
prettyPrintAsm = unlines . map one
  where one (Left s) = s ++ ":"
        one (Right (Imm (LitInt x))) = "    imm " ++ show x
        one (Right (Imm (LitLbl s))) = "    imm " ++ s
        one (Right i) = "    " ++ map toLower (show i)
