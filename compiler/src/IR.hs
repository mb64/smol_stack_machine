module IR where

import Data.List
import Text.Read

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
primName "ret" = Just Ret
primName "swap" = Just Swap
primName "dup" = Just Dup
primName "nop" = Just Nop
primName "if" = Just If
primName "ifelse" = Just IfElse
primName name
    | "rot" `isPrefixOf` name = Rot <$> readMaybe (drop 3 name)
    | "rotb" `isPrefixOf` name = RotB <$> readMaybe (drop 4 name)
    | otherwise = Nothing
