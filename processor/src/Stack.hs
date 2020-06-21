{-# LANGUAGE LambdaCase #-}

module Stack (StackAction(..), stack) where

import CustomPrelude
import BlockRam

data StackAction =
    SOne WordBits
    -- ^ SOne x : a b -- x
    | STwo WordBits WordBits
    -- ^ STwo x y : a b -- x y
    | SUnary WordBits
    -- ^ SUnary x : a -- x
    | SNew WordBits
    -- ^ SNew x : -- x
    | SDrop
    -- ^ SDrop : a --
    | SResurrect
    -- ^ SResurrect : -- a
    | SNop
    -- ^ SNop : -- 
    deriving (Show, Eq, Ord)

stack :: HiddenClockResetEnable dom
    => Signal dom StackAction -> Signal dom (WordBits, WordBits)
stack sa =
    let writes = flip fmap sa $ \case
            SOne x -> (Nothing, Just x)
            STwo x y -> (Just x, Just y)
            SUnary x -> (Nothing, Just x)
            SNew x -> (Nothing, Just x)
            SDrop -> (Nothing, Nothing)
            SResurrect -> (Nothing, Nothing)
            SNop -> (Nothing, Nothing)
        addr = stackPointer sa
    in fmap split $ blockRam addr writes

stackPointer :: HiddenClockResetEnable dom
    => Signal dom StackAction -> Signal dom (Unsigned 16)
stackPointer = mealy (\a b -> twice (f a b)) 0
  where f sp (SOne _) = sp - 1
        f sp (STwo _ _) = sp
        f sp (SUnary _) = sp
        f sp (SNew _) = sp + 1
        f sp SDrop = sp - 1
        f sp SResurrect = sp + 1
        f sp SNop = sp
        twice x = (x,x)
