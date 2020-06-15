-- Control flow graph
{-# LANGUAGE LambdaCase #-}

module IR.CFG
    ( SegId
    , Cont(..)
    , Segment(..)
    , Prog(..)
    , graphGen
    , optPrim
    , simplProg
    , validateProg
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord

import IR
import Assembly (litAdd, litNot)

type SegId = Int

-- Continuations
-- IfC indicates that the stack will end with a condition on top
data Cont = IfC SegId SegId
          | Always SegId
          | Ret
          deriving (Show, Eq, Ord)

data Segment = Segment SegId [Prim] Cont
             deriving (Show, Eq, Ord)

data Routine = Routine { entrySeg :: SegId, segments :: [Segment]}
             deriving (Show, Eq, Ord)

data Prog = Prog (Map.Map String SegId) [Segment]
          deriving (Show, Eq, Ord)

type SegIdGenM = State Int

genId :: SegIdGenM SegId
genId = state $ \i -> (i, i+1)

graphGenExprC :: Cont -> Expr -> SegIdGenM Routine
graphGenExprC cont e = case prims e of
    (ps, []) -> do
        sId <- genId
        pure $ Routine sId [Segment sId ps cont]
    (ps, IfElse ifExp elExp:rest) -> do
        sId <- genId
        Routine restSeg sr <- graphGenExprC cont rest
        Routine ifSeg si <- graphGenExprC (Always restSeg) ifExp
        Routine elSeg se <- graphGenExprC (Always restSeg) elExp
        let segment = Segment sId ps (IfC ifSeg elSeg)
        pure $ Routine sId $ [segment] ++ sr ++ si ++ se
    (_, Prim _:_) -> error "unreachable"
  where prims :: Expr -> ([Prim], Expr)
        prims (Prim p:as) = let (ps, as') = prims as in (p:ps, as')
        prims as = ([], as)

graphGenExpr :: Expr -> SegIdGenM Routine
graphGenExpr = graphGenExprC Ret

graphGen :: [Def] -> Prog
graphGen defs = fst $ runState prog 0
  where one :: Def -> WriterT [Segment] SegIdGenM (String, SegId)
        one (Def n e) = WriterT $ do
            Routine segId segs <- graphGenExpr e
            pure ((n, segId), segs)
        prog :: SegIdGenM Prog
        prog = do
            (segMap, segs) <- runWriterT $ Map.fromList <$> traverse one defs
            pure $ Prog segMap segs

-- Simple peephole opts on Prim, including some simple constant folding
optPrim' :: [Prim] -> [Prim]
optPrim' (Lit x:Not:rest) = optPrim' $ Lit (litNot x):rest
optPrim' (Lit x:Lit y:Add:rest) = optPrim' $ Lit (litAdd x y):rest
optPrim' (Lit x:Lit y:Swap:rest) = optPrim' $ Lit y:Lit x:rest
optPrim' (Add:Drop:rest) = optPrim' $ Drop:Drop:rest
optPrim' (Not:Drop:rest) = optPrim' $ Drop:rest
optPrim' (Lit _:Drop:rest) = optPrim' rest
optPrim' (Lit x:Dup:rest) = optPrim' $ Lit x:Lit x:rest
    -- ^ enable more constant folding; will be un-done in assembly
optPrim' (Dup:Drop:rest) = optPrim' rest
optPrim' (Swap:Drop:Drop:rest) = optPrim' $ Drop:Drop:rest
optPrim' (Swap:Add:rest) = optPrim' $ Add:rest
optPrim' (Swap:Swap:rest) = optPrim' rest
optPrim' (Not:Not:rest) = optPrim' rest
optPrim' (Rot x:RotB y:rest) | x == y = optPrim' rest
optPrim' (RotB x:Rot y:rest) | x == y = optPrim' rest
optPrim' (x:xs) = x:optPrim' xs
optPrim' [] = []

-- Keep doing optPrim' until it stops being optimized
-- This might make some impact if there's e.g. 1 2 swap add
optPrim :: [Prim] -> [Prim]
optPrim ps = let ps' = optPrim' ps
             in if ps == ps' then ps else optPrim ps'

mergeIdentical :: Prog -> Prog
mergeIdentical (Prog segMap segs) = Prog newSegMap newSegs
  where segKey (Segment _ p c) = (p, c)
        getId (Segment i _ _) = i

        segs' :: [[Segment]]
        segs' = groupBy ((==) `on` segKey)
            $ sortBy (comparing segKey) segs

        replaceMap :: Map.Map SegId SegId
        replaceMap = Map.fromList $ concatMap repls segs'

        repls :: [Segment] -> [(SegId, SegId)]
        repls (s:ss) = map (\s' -> (getId s', getId s)) ss
        repls [] = error "internal error: empty group"

        replace x = Map.findWithDefault x x replaceMap
        replaceC (IfC x y) = let a = replace x
                                 b = replace y
                             in if a == b then Always a else IfC a b
        replaceC (Always x) = Always (replace x)
        replaceC Ret = Ret

        newSegMap = fmap replace segMap
        newSegs = map (\(Segment i p c) -> Segment i p $ replaceC c)
            $ filter (\(Segment i _ _) -> i `Map.notMember` replaceMap) segs

tailCallOpt :: Prog -> Prog
tailCallOpt (Prog segMap segs) = Prog segMap $ map tco segs
  where tco (Segment i p@(_:_) Ret) = case last p of
            Call f -> Segment i (init p) (Always $ segMap Map.! f)
            _ -> Segment i p Ret
        tco s = s

-- TODO: constant condition

simplEmpty :: Prog -> Prog
simplEmpty (Prog segMap segs) = Prog newMap newSegs
  where (Endo segTrans, Endo contTrans) = foldMap one segs
        newMap = fmap segTrans segMap
        newSegs = map (\(Segment i ps c) -> Segment i ps (contTrans c)) segs

        trans :: SegId -> SegId -> SegId -> SegId
        trans a b x = if x == a then b else x

        one :: Segment -> (Endo SegId, Endo Cont)
        one (Segment segId [] (Always newId)) =
            let f = trans segId newId
            in (Endo f
               ,Endo $ \case
                  IfC x y -> IfC (f x) (f y)
                  Always x -> Always (f x)
                  Ret -> Ret
               )
        one (Segment segId [] Ret) =
            (Endo id
            ,Endo $ \case
                Always i | i == segId -> Ret
                x -> x
            )
        one _ = mempty

simplProg :: Prog -> Prog
simplProg = simplEmpty . tailCallOpt . mergeIdentical . optProg
  where optProg (Prog m ss) = Prog m $ map optSeg ss
        optSeg (Segment i p c) = Segment i (optPrim p) c

validateProg :: Prog -> Bool
validateProg (Prog segMap segs) = all exists segMap && all goodSeg segs
  where exists :: SegId -> Bool
        exists i = any (\(Segment i' _ _) -> i == i') segs

        goodCont :: Cont -> Bool
        goodCont (IfC i e) = exists i && exists e && i /= e
        goodCont (Always i) = exists i
        goodCont Ret = True

        goodSeg :: Segment -> Bool
        goodSeg (Segment _ _ c) = goodCont c
