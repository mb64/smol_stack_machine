module IR.Layout (ProgLayout, layout) where

import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.IntMap.Strict as IMap
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord

import IR.CFG

data ProgLayout = ProgLayout (Map.Map String SegId) [Segment]
                deriving (Show, Eq, Ord)

type SegMap = IMap.IntMap [Segment] -- Map SegId [Segment]

-- Not just Map.fromList bc it also eliminates dead code
-- It keeps everything reachable from some named segment
genSegMap :: Prog -> SegMap
genSegMap (Prog nameMap segs) = execState (traverse_ go nameMap) mempty
  where go :: SegId -> State SegMap ()
        go i = do
            let Segment _ p c = badSegMap IMap.! i
            m <- get
            if i `IMap.member` m
            then pure ()
            else do
                modify $ IMap.insert i [Segment i p c]
                case c of
                    IfC a b -> go a >> go b
                    Always a -> go a
                    Ret -> pure ()
        badSegMap :: IMap.IntMap Segment -- has dead code
        badSegMap = IMap.fromList $ map (\(Segment i p c) -> (i, Segment i p c)) segs

genRefMap :: [Segment] -> IMap.IntMap [(SegId, Cont)]
genRefMap segs = IMap.fromListWith (++) $ map (second pure) refs
  where refs :: [(SegId, (SegId, Cont))]
        refs = concatMap getRefs segs

        getRefs (Segment i _ (IfC a b)) = [(a, (i, IfC a b)), (b, (i, IfC a b))]
        getRefs (Segment i _ (Always a)) = [(a, (i, Always a))]
        getRefs (Segment _ _ Ret) = []

genTieredRefs :: [Segment] -> [(SegId, [SegId])]
genTieredRefs =
    map (second $ map fst)
    . sortBy (comparing tier')
    . map (\(i, is) -> (i, sortBy (comparing $ tierOne i . snd) is)) -- put Always's in front
    . IMap.toList
    . genRefMap
  where tier' :: (SegId, [(SegId, Cont)]) -> [Int]
        tier' = tier . second (map snd)

        tier :: (SegId, [Cont]) -> [Int]
        tier (i,cs) = map (tierOne i) cs

        tierOne :: SegId -> Cont -> Int
        tierOne _ (Always _) = 1
        tierOne x (IfC i _) = if x == i then 2 else 3
        tierOne _ Ret = 4

layout :: Prog -> ProgLayout
layout (Prog nameMap segs) = ProgLayout nameMap segs'
  where initialSegMap = genSegMap $ Prog nameMap segs
        initialBackToFront = IMap.mapWithKey const initialSegMap
        tieredRefs = genTieredRefs $ concat initialSegMap

        one :: (SegId, [SegId])
            -> IMap.IntMap SegId
            -> SegMap
            -> (IMap.IntMap SegId, SegMap)
        one (_, []) backToFront segMap = (backToFront, segMap)
        one (i, b:bs) backToFront segMap = case IMap.lookup i segMap of
            Just ss -> case IMap.updateLookupWithKey (\_ _ -> Nothing) b backToFront of
                (Just f, btf') -> (btf', IMap.update (Just . (++ss)) f $ IMap.delete i segMap)
                (Nothing, _) -> one (i, bs) backToFront segMap
            Nothing -> one (i, bs) backToFront segMap

        (_endBackToFront, endSegMap) = foldl
            (\(bf, sm) s -> one s bf sm)
            (initialBackToFront, initialSegMap)
            tieredRefs

        -- TODO actually figure out some notion of [Segment] affinity and group
        -- them appropriately
        groupForLocality :: [[Segment]] -> [Segment]
        groupForLocality = concat

        segs' :: [Segment]
        segs' = groupForLocality $ toList endSegMap

-- general algorithm ideas:
-- tiers of backreference priorities:
--  1. Always
--  2. IfC, it's the If
--  3. IfC, it's the Else
-- Sort the refMap by this, then go through, building up:
--  - A Map BackSegId FrontSegId (IntMap SegId)
--      starts as [(a,a), (b,b), (c,c), ...]
--      contains everything that is a back segment, and its corresponding front
--  - A Map FrontSegId [Segment] (IntMap [Segment])
--      starts as segMap
--      contains everything that is a front segment, and its list of segments
-- (for lookup/modify in log time of both BackSegId's and FrontSegId's)
-- Try to sort the [[Segment]] in some way to improve locality?
--  - TODO
