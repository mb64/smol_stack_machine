module Alu (AluAction(..), alu, aluActionFromBits) where

import CustomPrelude
import Stack

data AluAction = AAdd
               | ANot
               | ASwap
               deriving (Show, Eq, Ord)

aluActionFromBits :: BitVector 2 -> AluAction
aluActionFromBits $(bitPattern "00") = AAdd
aluActionFromBits $(bitPattern "01") = ANot
aluActionFromBits $(bitPattern "10") = ASwap
aluActionFromBits _ = undefined

alu :: AluAction -> (WordBits, WordBits) -> StackAction
alu AAdd (x, y) = SOne $ x + y
alu ANot (_, y) = SUnary $ complement y
alu ASwap (x, y) = STwo y x
