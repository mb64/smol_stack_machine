module BlockRam (blockRam) where

import CustomPrelude

import Clash.Annotations.Primitive
import Data.Bifunctor (first)
import Data.Maybe
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

-- | A block ram in WRITE_FIRST mode, with 16-bit addressing and 16-bit read and write
blockRam :: HiddenClockResetEnable dom
    => Signal dom (Unsigned 16) -- ^ Address
    -> Signal dom (Maybe (BitVector 8), Maybe (BitVector 8)) -- ^ Write first byte, Write second byte
    -> Signal dom (BitVector 16)
blockRam addr d = blockRam' addr writeData writeEnable
  where (writeData, writeEnable) = unbundle $ fmap f d
        f (b0, b1) =
            let wData = fromJust b0 ++# fromJust b1
                wEnable = (isJust b0, isJust b1)
            in (wData, wEnable)

-- Hardware impl.
{-# ANN blockRam' (InlinePrimitive [VHDL] $ unindent [i|
    [{"BlackBox": {
        "name": "BlockRam.blockRam'",
        "kind": "Declaration",
        "template":
"
-- BEGIN: VHDL for the blockram
-- Uhh
-- TODO
-- END: VHDL for the blockram
"
    }
    }]
|]) #-}
{-# NOINLINE blockRam' #-}
blockRam' :: HiddenClockResetEnable dom
    => Signal dom (Unsigned 16)  -- ^ Address
    -> Signal dom (BitVector 16) -- ^ Data to write
    -> Signal dom (Bool, Bool)   -- ^ Write enable (first byte, second byte)
    -> Signal dom (BitVector 16)
blockRam' addr writeData writeEnable = blockRamImpl $
    f <$> fmap fromIntegral addr
      <*> fmap split writeData
      <*> writeEnable
  where f addr' (b0, b1) (e0, e1) = 
            let w0 = if e0 then Just b0 else Nothing
                w1 = if e1 then Just b1 else Nothing
            in (addr', w0, w1)

-- Software impl.
-- We need the state to be a NFDataX, which rules out obvious choices (Array, IntMap)
-- Instead, we're stuck with a linear search through a list.  Oh well.
type BlockRamData = [(Int, BitVector 8)]

blockRamImpl :: HiddenClockResetEnable dom
    => Signal dom (Int, Maybe (BitVector 8), Maybe (BitVector 8))
    -> Signal dom (BitVector 16)
blockRamImpl inputs = mealy fun [] inputs
  where fun :: BlockRamData
            -> (Int, Maybe (BitVector 8), Maybe (BitVector 8))
            -> (BlockRamData, BitVector 16)
        fun dat (addr, w0, w1) = let (dat', r0) = lookupReplace addr w0 dat
                                     (dat'', r1) = lookupReplace (addr + 1) w1 dat'
                                 in (dat'', r0 ++# r1)

        lookupReplace
            :: Int
            -> Maybe (BitVector 8)
            -> BlockRamData
            -> (BlockRamData, BitVector 8)
        lookupReplace addr replacement ((ta,td):dat)
            | ta == addr = let new = fromMaybe td replacement in ((addr,new):dat, new)
            | otherwise = first ((ta,td):) $ lookupReplace addr replacement dat
        lookupReplace addr (Just new) [] = ([(addr, new)], new)
        lookupReplace _ Nothing [] = ([], 0)


