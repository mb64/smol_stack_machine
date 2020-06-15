module Assembly where

import Data.Bifunctor
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

data Assembly = Assembly [Instr Lit] [(Label, [Instr Lit])]
              deriving (Show, Eq, Ord)

instance Semigroup Assembly where
    Assembly a [] <> Assembly b bs = Assembly (a ++ b) bs
    Assembly a as <> Assembly [] bs = Assembly a (as ++ bs)
    Assembly a as <> Assembly b bs = Assembly a (onLast (second (++b)) as ++ bs)
      where onLast _ [] = error "last: empty list"
            onLast f [x] = [f x]
            onLast f (x:xs) = x:onLast f xs

instance Monoid Assembly where
    mempty = Assembly [] []

oneInstr :: Instr Lit -> Assembly
oneInstr i = Assembly [i] []

oneLabel :: Label -> Assembly
oneLabel l = Assembly [] [(l,[])]

prettyPrintAsm :: Assembly -> String
prettyPrintAsm (Assembly a as) = unlines $ map oneI a ++ map one as
  where oneL s = s ++ ":"

        oneI (Imm (LitInt x)) = "    imm " ++ show x
        oneI (Imm (LitLbl s)) = "    imm " ++ s
        oneI i = "    " ++ map toLower (show i)

        one (l, is) = unlines $ oneL l : map oneI is
