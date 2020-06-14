{
-- vim:ft=happy
{-# LANGUAGE DataKinds #-}

module Parser (parse) where

import Data.Word

import AST
import Lexer
}

%name parse
%tokentype {Token}
%error {parseError}

%token
    '{'     {TOpenBrace}
    '}'     {TCloseBrace}
    '('     {TOpenParen}
    ')'     {TCloseParen}
    ':'     {TColon}
    '--'    {TStackSep}
    'def'   {TDef}
    'macro' {TMacro}
    'typedef' {TTypeDef}
    'if'    {TIf}
    'ifelse' {TIfElse}
    lit     {TLit $$}
    ident   {TIdent $$}
    quot    {TQuotedIdent $$}

%%

Res     :: {[Def 'True 'True]}
        :           {[]}
        | Def Res   {$1:$2}


Lit     :: {Word8}
        : lit       {toWord $1}

IfElse  :: {Atom 'True}
        : '{' Expr '}' 'if'                     {IfElse $2 []}
        | '{' Expr '}' '{' Expr '}' 'ifelse'    {IfElse $2 $5}

Atom    :: {Atom 'True}
        : ident                 {Name $1}
        | quot                  {LitName $1}
        | Lit                   {Lit $1}
        | IfElse                {$1}
        | '{' FunType Expr '}'  {Lambda $2 $3}

Expr    :: {Expr 'True}
        :               {[]}
        | Atom Expr     {$1:$2}

FunType :: {Type}
        : '(' Types '--' Types ')'  {FunT $2 $4}

Type    :: {Type}
        : ident     {NameT $1}
        | FunType   {$1}

Types   :: {[Type]}
        :               {[]}
        | Type Types    {$1:$2}

Def     :: {Def 'True 'True}
        : 'def' ident ':' FunType '{' Expr '}'  {Def $2 $4 $6}
        | 'macro' ident '{' Expr '}'            {Macro $2 $4}
        | 'typedef' ident Type                  {TypeDef $2 $3}

{
parseError :: [Token] -> a
parseError xs = error $ "Failed to parse at tokens " ++ show (take 3 xs)

toWord :: Integer -> Word8
toWord x
    | x >= 256 || x < -128 = error $ "Literal " ++ show x ++ "out of bounds"
    | otherwise = fromIntegral x
}
