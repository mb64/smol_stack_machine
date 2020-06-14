module IR.CodeGen where

import Control.Monad.State

import IR
import IR.CFG
import Assembly (Assembly, Label(..), Lit(..))

type LblGenM = State Int

compileDef :: Def -> LblGenM Assembly
compileDef = error "TODO"
