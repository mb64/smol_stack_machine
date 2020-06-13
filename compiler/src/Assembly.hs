module Assembly where

import Data.Char
import Data.Word

data Lit = LitInt Word8
         | LitLbl Label
         -- | LitArith Label (Word8 -> Word8)
         deriving (Show, Eq, Ord)

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

data Label = Lbl String
           | GenLbl Int
           deriving (Show, Eq, Ord)

type Assembly = [Either Label (Instr Lit)]

prettyPrintAsm :: Assembly -> String
prettyPrintAsm = unlines . map one
  where one (Left (Lbl s)) = s ++ ":"
        one (Left (GenLbl n)) = ".l" ++ show n ++ ":"
        one (Right (Imm (LitInt x))) = "    imm " ++ show x
        one (Right (Imm (LitLbl (Lbl s)))) = "    imm " ++ s
        one (Right (Imm (LitLbl (GenLbl n)))) = "    imm .l" ++ show n
        one (Right i) = "    " ++ map toLower (show i)
