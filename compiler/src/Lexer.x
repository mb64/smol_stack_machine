{ -- vim:ft=alex
module Lexer (lexText, Token(..)) where
}

%wrapper "basic"

$identchar = [A-Za-z0-9_\-]

@declit = "-"? [0-9][0-9_]*
@hexlit = "-"? "0"[Xx][0-9a-fA-F][0-9A-Fa-f_]*

tokens :-
    $white+         ;
    "//".*          ;
    @declit         {TLit . read . filter (/= '_')}
    @hexlit         {TLit . read . filter (/= '_')}
    "{"             {const TOpenBrace}
    "}"             {const TClosedBrace}
    "def"           {const TDef}
    "+"             {TIdent}
    "-"             {TIdent}
    "*"             {TIdent}
    "'"$identchar+  {TQuotedIdent . tail}
    $identchar+     {TIdent}

{
data Token = TDef
    | TOpenBrace | TClosedBrace
    | TLit Integer
    | TIdent String
    | TQuotedIdent String
    deriving (Show, Eq, Ord)

lexText :: String -> [Token]
lexText = alexScanTokens
}
