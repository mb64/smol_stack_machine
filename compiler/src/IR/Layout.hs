module IR.Layout (ProgLayout, layout) where

import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map.Strict as Map

import IR
import IR.CFG

data ProgLayout = ProgLayout (Map.Map String SegId) [Segment]
                deriving (Show, Eq, Ord)

type SegMap = Map.Map SegId ([Prim], Cont)
type RefMap = Map.Map SegId [SegId]

-- Not just Map.fromList bc it also eliminates dead code
-- It keeps everything reachable from some named segment
genSegMap :: [Segment] -> SegMap
genSegMap (Prog nameMap segs) = execState (traverse_ go nameMap) mempty
  where go :: Segment -> State SegMap ()
        go (Segment i p c) = do
            m <- get
            if i `Map.member` m
            then pure ()
            else do
                modify $ Map.insert i (p,c)
                case c of
                    IfC a b -> go a >> go b
                    Always a -> go a
                    Ret -> pure ()

genRefMap :: [Segment] -> RefMap
genRefMap segs = Map.fromListWith (++) $ map (second pure) refs
  where refs :: [(SegId, SegId)]
        refs = concatMap getRefs segs

        getRefs (Segment i _ IfC a b) = [(a, i), (b, i)]
        getRefs (Segment i _ Always a) = [(a, i)]
        getRefs (Segment i _ Ret) = []

layout :: Prog -> ProgLayout
layout (Prog nameMap segs) = ProgLayout nameMap -- TODO
  where segMap = genSegMap $ Prog nameMap segs
        refMap = genRefMap segs

        -- TODO
        --
        -- general algorithm ideas:
        -- tiers of backreference priorities:
        --  1. only 1 Always
        --  2. only 1 IfC (separated by if or else clause)
        --  3. Multiple, containing an Always
        --  4. Multiple, only IfC
        -- Sort the refMap by this, then go through, building up:
        --  - A Map (SegId? front/back?) [Segment]
        --  - A Set of used up SegIds (the ones that are already in front of
        --    another segment)
        --
        -- Things this won't quite do well:
        --  - if the ordering should change, after removing some reference
