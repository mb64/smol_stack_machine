module Main where

import Lexer
import Parser
import AST
import IR
import IR.CodeGen
import Assembly
import Assembly.Parser
import Binary

import qualified Data.ByteString.Lazy as B
import Data.Foldable
import System.Environment
import System.IO

compileProgram :: String -> Assembly
compileProgram prog = asm
  where ast = parse $ lexText prog
        ir = map lowerDef $ simplifyAst ast
        asm = compileIr ir

singleFile :: String -> IO Assembly
singleFile fileName = case dropWhile (/= '.') fileName of
    ".sm" -> fmap compileProgram $ readFile fileName
    ".s" -> fmap parseAssembly $ readFile fileName
    e -> error $ "Unrecognized file extension: " ++ e

main :: IO ()
main = do
    args <- getArgs
    asm <- fmap fold $ traverse singleFile args
    putStr $ prettyPrintAsm asm
    let bin = assemble asm
    withFile "out.bin" WriteMode $ \h -> do
        B.hPut h $ B.pack bin
