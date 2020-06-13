{-# LANGUAGE DataKinds #-}

module IR where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List
import Data.Word
import Text.Read

import qualified AST
import AST (Ident)
import qualified Assembly as Asm
import Assembly (Lit(..), Instr, Label(..))

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

type LblGenM = State Int

genLbl :: LblGenM Label
genLbl = state $ \n -> (GenLbl n, n+1)

compileDef :: Def -> LblGenM [Either Label (Instr Lit)]
compileDef (Def n e) = do
    e' <- traverse compileA e
    pure $ Left (Lbl n) : concat e'
  where compileA :: Atom -> LblGenM [Either Label (Instr Lit)]
        compileA (Lit x) = pure [Right $ Asm.Imm $ LitInt x]
        compileA (LitName i) = pure [Right $ Asm.Imm $ LitLbl (Lbl i)]
        compileA (Prim p) = compileP p

        compileP :: Prim -> LblGenM [Either Label (Instr Lit)]
        compileP Send = pure [Right Asm.Send]
        compileP Drop = pure [Right Asm.Drop]
        compileP Add = pure [Right Asm.Add]
        compileP Not = pure [Right Asm.Not]
        compileP Call = do
            l <- genLbl
            pure [ Right $ Asm.Imm (LitLbl l)
                 , Right $ Asm.Swap
                 , Right $ Asm.Jmp
                 , Left l
                 ]
        compileP Ret = pure [Right Asm.Jmp]
        compileP Swap = pure [Right Asm.Swap]
        compileP Dup = pure [Right Asm.Dup]
        compileP (Rot x)
            | x <= 1 = pure []
            | otherwise = pure . map Right
                $ replicate (x - 2) Asm.Drop
                ++ concat (replicate (x - 2) [Asm.Swap, Asm.Resurrect])
                ++ [Asm.Swap]
        compileP (RotB x)
            | x <= 1 = pure []
            | otherwise = pure . map Right
                $ concat (replicate (x - 2) [Asm.Swap, Asm.Drop])
                ++ [Asm.Swap]
                ++ replicate (x - 2) Asm.Resurrect
        compileP Nop = pure []
        compileP If = do
            l1 <- genLbl
            l2 <- genLbl
            pure [ Right $ Asm.Swap
                 , Right $ Asm.Imm (LitLbl l1)
                 , Right $ Asm.Swap
                 , Right $ Asm.Skip
                 , Right $ Asm.Jmp
                 , Right $ Asm.Drop
                 , Right $ Asm.Imm (LitLbl l2)
                 , Right $ Asm.Swap
                 , Right $ Asm.Jmp
                 , Left l1
                 , Right $ Asm.Drop
                 , Left l2
                 ]
        compileP IfElse = do
            l1 <- genLbl
            pure [ Right $ Asm.Drop
                 , Right $ Asm.Swap
                 , Right $ Asm.Resurrect
                 , Right $ Asm.Swap
                 , Right $ Asm.Skip
                 , Right $ Asm.Swap
                 , Right $ Asm.Drop
                 , Right $ Asm.Imm (LitLbl l1)
                 , Right $ Asm.Swap
                 , Right $ Asm.Jmp
                 , Left l1
                 ]

-- Rot  3 : ( a b c -- b c a )
-- RotB 3 : ( a b c -- c a b )
--
--
-- Two options for compiling `if`:
--
-- *** chosen option for now
-- cond addr
-- addr cond    |   swap
-- addr cond l1 |   imm l1
-- addr l1 cond |   swap
-- addr l1      |   skip jmp
-- addr         |   drop
-- addr l2      |   imm l2
-- l2 addr      |   swap
-- l2           |   jmp
-- addr         | l1:
--              |   drop
--              | l2:
--
-- cond addr
-- addr cond    |   swap
-- addr cond l1 |   imm l1
-- addr l1 cond |   swap        \
-- addr l1      |   drop        | rot 3
-- l1 addr      |   swap        |
-- l1 addr cond |   resurrect   /
-- l1 addr not  |   not
-- l1 addr      |   skip jmp
-- l1           |   drop
--              |   drop
--              | l1:
--
-- The third option, and the most legit one, is to make a control flow graph,
-- and optimize everything there
--
-- `ifelse`:
--
-- cond if else
-- cond if [else]   |   drop        \
-- if cond [else]   |   swap        | rotb 3
-- if cond else     |   ressurect   |
-- if else cond     |   swap        /
-- ??? ???          |   skip swap
-- ???              |   drop
-- ??? l1           |   imm l1
-- l1 ???           |   swap
--                  |   jmp
--                  | l1:
