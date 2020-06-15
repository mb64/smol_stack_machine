{
-- vim:ft=happy
module Assembly.Parser (parse) where

import Data.Char
import Data.Maybe

import Assembly
import Assembly.Lexer
}

%name parse
%tokentype {Token}
%error {parseError}

%token
    ':'     {TColon}
    'imm'   {TImm}
    name    {TName $$}

%%

Res     :: {Assembly}
        :           {mempty}
        | Label Res {oneLabel $1 <> $2}
        | Instr Res {oneInstr $1 <> $2}

Label   :: {Label}
        : name ':'      {parseLbl $1}

Instr   :: {Instr Lit}
        : 'imm' name    {Imm $ LitLbl $ parseLbl $2}
        | name          {parseInstr $ map toLower $1}

{
parseError :: [Token] -> a
parseError xs = error $ "Failed to parse at tokens " ++ show (take 3 xs)

tryParseInstr :: String -> Maybe (Instr Lit)
tryParseInstr "send" = Just Send
tryParseInstr "jmp" = Just Jmp
tryParseInstr "skip" = Just Skip
tryParseInstr "drop" = Just Drop
tryParseInstr "add" = Just Add
tryParseInstr "not" = Just Not
tryParseInstr "swap" = Just Swap
tryParseInstr "dup" = Just Dup
tryParseInstr "resurrect" = Just Resurrect
tryParseInstr "nop" = Just Nop
tryParseInstr _ = Nothing

parseInstr :: String -> Instr Lit
parseInstr i = fromMaybe (error $ "unrecognized instruction " ++ i) $ tryParseInstr i

-- TODO: should labels that are also instructions be allowed?
parseLbl :: String -> Label
parseLbl s = maybe s (const $ error $ s ++ " can't be a label, it's an instruction") $ tryParseInstr (map toLower s)
}
