module Main where

import Lexer
import Parser
import AST
import IR
import IR.CodeGen
import Assembly
import Binary

import System.IO
import qualified Data.ByteString.Lazy as B

compileProgram :: String -> Assembly
compileProgram prog = asm
  where ast = parse $ lexText prog
        ir = map lowerDef $ simplifyAst ast
        asm = compileIr ir

main :: IO ()
-- main = interact $ prettyPrintAsm . compileProgram
main = do
    prog <- getContents
    let asm = compileProgram prog
    putStr $ prettyPrintAsm asm
    let bin = assemble asm
    withFile "out.bin" WriteMode $ \h -> do
        B.hPut h $ B.pack bin
