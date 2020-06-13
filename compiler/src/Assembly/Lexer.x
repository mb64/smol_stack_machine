{
module Assembly.Lexer (lexText, Token(..)) where
}

%wrapper "basic"

$identchar = [A-Za-z0-9_\-]
$charlit = . # \' # \\

@declit = "-"? [0-9][0-9_]*
@hexlit = "-"? "0"[Xx][0-9a-fA-F][0-9A-Fa-f_]*

tokens :-
    $white+         ;
    ";".*           ;
    @declit         {TLit . read . filter (/= '_')}
    @hexlit         {TLit . read . filter (/= '_')}
    "'\''"          {const $ TLit $ fromIntegral $ ord '\''}
    "'\\'"          {const $ TLit $ fromIntegral $ ord '\\'}
    "'\n'"          {const $ TLit $ fromIntegral $ ord '\n'}
    "'\t'"          {const $ TLit $ fromIntegral $ ord '\t'}
    "'"$charlit"'"  {TLit . fromIntegral . ord . head . tail}
    ":"             {const TColon}
    "imm"           {const TImm}
    $identchar+     {TName}

{
data Token = TLit Word8
           | TName String
           | TImm -- Special syntax, special token
           | TColon
           deriving (Show, Eq, Ord)

lexText :: String -> [Token]
lexText = alexScanTokens
}
