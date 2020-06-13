module Assembly where

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
