{ -- vim:ft=happy
module Parser (parse) where

import AST
import Lexer

import Data.Word
}

%name parse
%tokentype {Token}
%error {parseError}

%token
    '{'     {TOpenBrace}
    '}'     {TClosedBrace}
    'def'   {TDef}
    lit     {TLit $$}
    ident   {TIdent $$}
    quot    {TQuotedIdent $$}

%%

Res     :: {[Def]}
        :           {[]}
        | Def Res   {$1:$2}


Lit     :: {Word8}
        : lit       {toWord $1}

Atom    :: {Atom}
        : ident         {Name $1}
        | quot          {LitName $1}
        | Lit           {Lit $1}
        | '{' Expr '}'  {Lambda $2}

Expr    :: {Expr}
        :               {[]}
        | Atom Expr     {$1:$2}

Def     :: {Def}
        : 'def' ident '{' Expr '}'  {Def $2 $4}

{
parseError :: [Token] -> a
parseError xs = error $ "Failed to parse at tokens " ++ show (take 3 xs)

toWord :: Integer -> Word8
toWord x
    | x >= 256 || x < -128 = error $ "Literal " ++ show x ++ "out of bounds"
    | otherwise = fromIntegral x
}
