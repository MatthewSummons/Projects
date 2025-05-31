{
module Parser (parseProg, parseType, parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
}


%name parserProg Program
%name parserType typ
%name parserExpr Exp
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar }
    id      { TokenSym $$ }
    int     { TokenInt $$ }
    Int     { TokenTInt }
    Bool    { TokenTBool }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '}'     { TokenRBrace }
    '{'     { TokenLBrace }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    ';'     { TokenSemiColon }
    ':'     { TokenColon }
    ','     { TokenComma }
    '.'     { TokenPeriod }
    '='     { TokenEq }
    if      { TokenIf }
    else    { TokenElse }
    true    { TokenTrue }
    false   { TokenFalse }
    '<'     { TokenLT }
    '<='    { TokenLE }
    '>'     { TokenGT }
    '>='    { TokenGE }
    '=='    { TokenComp }
    '&&'    { TokenAnd }
    '!'     { TokenNot }
    '!!'    { TokenIndex }
    '||'    { TokenOr }
    fun     { TokenFunc }

%right ';' else
%left '||'
%left '&&'
%nonassoc '=='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left '!!' '.'
%left NEG NOT

%monad { Either String }


%%

Program ::                     { Program }
        : Functions Exp        { Program $1 $2 }

Functions ::                    { [(String, Function)] }
          : Functions Function  { $1 ++ [$2] }
          | {- empty -}         { [] }

Function ::                                 { (String, Function) }
         : fun id '(' ids ')' '{' Exp '}'   { ($2, Function $4 $7) }

ids ::                      { [(String, Type)] }
    : ids ',' id ':' typ    { $1 ++ [($3, $5)] }
    | id ':' typ            { [($1, $3)] }
    | {- empty -}           { [] }

Fields ::                        { [(Label, Exp)] }
       : Fields ',' id '=' Exp   { $1 ++ [($3, $5)] }
       | id '=' Exp              { [($1, $3)] }
       | {- empty -}             { [] }

typ ::            { Type }
    : Int         { TInt }
    | Bool        { TBool }
    | '[' typ ']' { TArray $2 }
    | '{' ids '}' { TRecord $2 }

Exp ::                                { Exp }
    : var id '=' Exp ';' Exp          { Decl $2 $4 $6 }
    | if '(' Exp ')' Exp ';' else Exp { If $3 $5 $8 }
    | Oper                            { $1 }

Oper ::                               { Exp }
     : Oper '||' Oper                 { Bin Or $1 $3 }
     | Oper '&&' Oper                 { Bin And $1 $3 }
     | Oper '==' Oper                 { Bin EQ $1 $3 }
     | Oper '<' Oper                  { Bin LT $1 $3 }
     | Oper '>' Oper                  { Bin GT $1 $3 }
     | Oper '<=' Oper                 { Bin LE $1 $3 }
     | Oper '>=' Oper                 { Bin GE $1 $3 }
     | Oper '+' Oper                  { Bin Add $1 $3 }
     | Oper '-' Oper                  { Bin Sub $1 $3 }
     | Oper '*' Oper                  { Bin Mult $1 $3 }
     | Oper '/' Oper                  { Bin Div $1 $3 }
     | '-' Oper %prec NEG             { Unary Neg $2 }
     | '!' Oper %prec NOT             { Unary Not $2 }
     | App                            { $1 } 

App ::                                { Exp }
    : id '(' Elements ')'             { Call $1 $3 }
    | '{' Fields '}'                  { Record $2 }
    | App '.' id                      { Proj $1 $3 }
    | '[' Elements ']'                { Array $2 }
    | App '!!' App                    { Index $1 $3 }
    | id                              { Var $1 }
    | int                             { Lit (IntV $1) }
    | true                            { Lit (BoolV True) }
    | false                           { Lit (BoolV False) }
    | '(' Exp ')'                     { $2 }

Elements ::                           { [Exp] }
         : Elements ',' Exp           { $1 ++ [$3] }
         | Exp                        { [$1] }
         | {- empty -}                { [] }

{

parseError :: [Token] -> Either String a
parseError _ = Left "Parse error"

parseProg :: String -> Either String Program
parseProg = parserProg . scanTokens

parseType :: String -> Either String Type
parseType = parserType . scanTokens

parseExpr :: String -> Either String Exp
parseExpr = parserExpr . scanTokens

}
