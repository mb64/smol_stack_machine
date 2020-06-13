module Main where

import Control.Monad.State

import Lexer
import Parser
import AST
import IR
import Assembly

compileProgram :: String -> Assembly
compileProgram prog = asm
  where ast = parse $ lexText prog
        ir = map lowerDef $ simplifyAst ast
        asm = concat $ fst $ runState (traverse compileDef ir) 0

main :: IO ()
main = interact $ prettyPrintAsm . compileProgram
