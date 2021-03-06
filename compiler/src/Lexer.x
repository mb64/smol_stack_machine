{
module Lexer (lexText, Token(..)) where
}

%wrapper "basic"

$identchar = [A-Za-z0-9_\-]
$charlit = . # \' # \\

@declit = "-"? [0-9][0-9_]*
@hexlit = "-"? "0"[Xx][0-9a-fA-F][0-9A-Fa-f_]*

tokens :-
    $white+         ;
    "//".*          ;
    @declit         {TLit . read . filter (/= '_')}
    @hexlit         {TLit . read . filter (/= '_')}
    "'\''"          {const $ TLit $ fromIntegral $ ord '\''}
    "'\\'"          {const $ TLit $ fromIntegral $ ord '\\'}
    "'\n'"          {const $ TLit $ fromIntegral $ ord '\n'}
    "'\t'"          {const $ TLit $ fromIntegral $ ord '\t'}
    "'"$charlit"'"  {TLit . fromIntegral . ord . head . tail}
    "{"             {const TOpenBrace}
    "}"             {const TCloseBrace}
    "("             {const TOpenParen}
    ")"             {const TCloseParen}
    ":"             {const TColon}
    "--"            {const TStackSep}
    "+"             {const $ TIdent "add"}
    "-"             {const $ TIdent "sub"}
    "*"             {const $ TIdent "mul"}
    "def"           {const TDef}
    "macro"         {const TMacro}
    "typedef"       {const TTypeDef}
    "if"            {const TIf}
    "ifelse"        {const TIfElse}
    "'"$identchar+  {TQuotedIdent . tail}
    $identchar+     {TIdent}

{
data Token = TDef | TMacro | TTypeDef
    | TOpenBrace | TCloseBrace
    | TOpenParen | TCloseParen
    | TStackSep | TColon
    | TIf | TIfElse
    | TLit Integer
    | TIdent String
    | TQuotedIdent String
    deriving (Show, Eq, Ord)

lexText :: String -> [Token]
lexText = alexScanTokens
}
