module Main where

import Lexer
import Parser
import AST
import IR
import IR.CodeGen
import Assembly

compileProgram :: String -> Assembly
compileProgram prog = asm
  where ast = parse $ lexText prog
        ir = map lowerDef $ simplifyAst ast
        asm = compileIr ir

main :: IO ()
main = interact $ prettyPrintAsm . compileProgram
