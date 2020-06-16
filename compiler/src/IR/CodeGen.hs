module IR.CodeGen (compileIr, compileLayout) where

import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map

import IR (Prim)
import qualified IR
import IR.CFG
import IR.Layout
import Assembly

type LblGenM = State Int

genLbl :: LblGenM Label
genLbl = state $ \l -> (".c" ++ show l, l+1)

-- Compiles straight-line code (no branches)
compileLinear :: [Prim] -> LblGenM Assembly
compileLinear = fmap fold . traverse one
  where one :: Prim -> LblGenM Assembly
        one IR.Send = pure $ oneInstr Send
        one IR.Drop = pure $ oneInstr Drop
        one IR.Not = pure $ oneInstr Not
        one IR.Add = pure $ oneInstr Add
        one (IR.Call f) = do
            l <- genLbl
            pure $ Assembly
                [ Imm (LitLbl l)
                , Imm (LitLbl f)
                , Jmp
                ]
                [(l, [])]
        one IR.VCall = do
            l <- genLbl
            pure $ Assembly
                [ Imm (LitLbl l)
                , Swap
                , Jmp
                ]
                [(l, [])]
        one (IR.Lit x) = pure $ oneInstr $ Imm x
        one IR.Swap = pure $ oneInstr Swap
        one IR.Dup = pure $ oneInstr Dup
        one (IR.Rot x)
            | x <= 1 = pure mempty
            | otherwise = pure . instrs
                $ replicate (x - 2) Drop
                ++ concat (replicate (x - 2) [Swap, Resurrect])
                ++ [Swap]
        one (IR.RotB x)
            | x <= 1 = pure mempty
            | otherwise = pure . instrs
                $ concat (replicate (x - 2) [Swap, Drop])
                ++ [Swap]
                ++ replicate (x - 2) Resurrect

-- Most optimization is already done on [Prim] with optPrim
-- There's just a few things left to bother with
optAsm :: [Instr Lit] -> [Instr Lit]
optAsm (Swap:Swap:rest) = optAsm rest
optAsm (Imm x:Imm y:Swap:rest) = optAsm $ Imm y:Imm x:rest
optAsm (Not:Not:rest) = optAsm rest
optAsm (i:is) = i:optAsm is
optAsm [] = []

optAssembly :: Assembly -> Assembly
optAssembly (Assembly a as) = Assembly (optAsm a) $ map (second optAsm) as

data Cont' = Next
           | JmpTo SegId
           | Ret'
           | NextOr SegId -- Continue on truthy condition, branch on falsy
           | Branch SegId SegId -- Branch tt ff
           deriving (Show, Eq, Ord)

data AsmSeg = AsmSeg SegId Assembly Cont'
            deriving (Show, Eq, Ord)

compileLayout :: ProgLayout -> Assembly
compileLayout (ProgLayout nameMap segs) = optAssembly $ compileAsmSegs idToLabel asmSegs
  where nameBackMap :: IMap.IntMap Label
        nameBackMap = IMap.fromList (map (\(l,i) -> (i,l)) $ Map.toList nameMap)

        idToLabel :: SegId -> Label
        idToLabel i = case IMap.lookup i nameBackMap of
            Just l -> l
            Nothing -> ".l" ++ show i

        compileSeg :: Segment -> LblGenM AsmSeg
        compileSeg (Segment i p c) = do
            a <- compileLinear p
            pure $ uncurry (AsmSeg i) $ case c of
                Always x | x == i + 1 -> (a, Next)
                Always x | otherwise -> (a, JmpTo x)
                IfC tt ff | tt == i + 1 -> (a, NextOr ff)
                IfC tt ff | ff == i + 1 -> (a <> oneInstr Not, NextOr tt)
                IfC tt ff | otherwise -> (a, Branch tt ff)
                Ret -> (a, Ret')

        asmSegs :: [AsmSeg]
        asmSegs = fst $ runState (traverse compileSeg segs) 0

safeToSkip :: Instr Lit -> Bool
safeToSkip (Imm (LitInt x)) | x >= 10 = True -- second byte is nop
safeToSkip (Imm _) = False
safeToSkip _ = True

compileAsmSegs :: (SegId -> Label) -> [AsmSeg] -> Assembly
compileAsmSegs getLbl asmSegs = go asmSegs
  where lbl :: SegId -> Assembly
        lbl i = oneLabel (getLbl i)

        go :: [AsmSeg] -> Assembly
        go (AsmSeg i a Next:rest) = lbl i <> a <> go rest
        go (AsmSeg i a (JmpTo x):rest) = lbl i <> a
            <> instrs [Imm (LitLbl (getLbl x)), Jmp] <> go rest
        go (AsmSeg i a Ret':rest) = lbl i <> a <> oneInstr Jmp <> go rest
        go (AsmSeg i a (NextOr x):rest@(AsmSeg _ (Assembly [instr] []) _:_))
            | x == i + 2 && safeToSkip instr =
                lbl i <> a <> instrs [Not, Skip] <> go rest
        go (AsmSeg i a (NextOr x):rest) = lbl i <> a
            <> instrs
            [ Imm (LitLbl (getLbl x))
            , Swap
            , Skip
            , Jmp
            , Drop
            ] <> go rest
        go (AsmSeg i a (Branch x y):rest) = lbl i <> a
            <> instrs
            [ Imm (LitLbl (getLbl x))
            , Swap
            , Imm (LitLbl (getLbl y))
            , Swap
            , Skip
            , Swap
            , Drop
            , Jmp
            ] <> go rest
        go [] = mempty

compileIr :: [IR.Def] -> Assembly
compileIr = compileLayout . layout . simplProg . graphGen
