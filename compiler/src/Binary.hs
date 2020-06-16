module Binary (resolveLabels, assemble) where

import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Word

import Assembly

size :: [Instr a] -> Int
size = sum . map sizeI
  where sizeI (Imm _) = 2
        sizeI _ = 1

genLabelMap :: Assembly -> Map.Map Label Word8
genLabelMap (Assembly a as) = if totalSize < 256
                              then fmap fromIntegral finalMap
                              else error "too many instructions"
  where go (pos, m) (l, is) = (pos + size is, Map.insert l pos m)
        (totalSize, finalMap) = foldl go (size a, mempty) as

resolveLabels :: Assembly -> [Instr Word8]
resolveLabels (Assembly a as) = map (fmap computeLit) allInstrs
  where labelMap = genLabelMap (Assembly a as)
        allInstrs = a ++ concatMap snd as

        computeLit :: Lit -> Word8
        computeLit (LitInt x) = x
        computeLit (LitLbl l) = case Map.lookup l labelMap of
            Just x -> x
            Nothing -> error $ "unresolved label: " ++ l
        computeLit (LitNot x) = complement $ computeLit x
        computeLit (LitAdd x y) = computeLit x + computeLit y

assemble :: Assembly -> [Word8]
assemble = concatMap toBinary . resolveLabels
  where toBinary :: Instr Word8 -> [Word8]
        toBinary Send = [0]
        toBinary Jmp = [1]
        toBinary Skip = [2]
        toBinary Drop = [3]
        toBinary Add = [4]
        toBinary Not = [5]
        toBinary Swap = [6]
        toBinary Dup = [7]
        toBinary Resurrect = [8]
        toBinary (Imm x) = [9, x]
        toBinary Nop = [10] -- which Nop to use? a question.
