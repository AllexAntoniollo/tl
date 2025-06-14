{
module Parser where 

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenAdd }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '!'             { TokenNot }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    '*'             { TokenMul }
    ','             { TokenComma }
    '/'             { TokenDiv }
    "=="            { TokenEq }
    '<'             { TokenLt }
    var             { TokenVar $$ }
    '\\'            { TokenLam }
    ':'             { TokenColon }
    "->"            { TokenArrow }
    Number          { TokenTNum }
    Boolean         { TokenTBool }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    '.'             { TokenDot }

%nonassoc if then else 
%nonassoc '\\' 
%left '+'
%left '*' 
%left '/'
%left "&&"
%left "||"
%left "=="
%left '<'
%left ','
%left '.'

%%

Exp     : num                           { Num $1 }
        | true                          { BTrue }
        | false                         { BFalse }
        | Exp '+' Exp                   { Add $1 $3 }
        | Exp '*' Exp                   { Mul $1 $3 }
        | Exp '/' Exp                   { Div $1 $3 }
        | Exp "&&" Exp                  { And $1 $3 }
        | Exp "||" Exp                  { Or $1 $3 }
        | Exp "==" Exp                  { Eq $1 $3 }
        | Exp '<' Exp                   { Lt $1 $3 }
        | '!' Exp                       { Not $2 }
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | var                           { Var $1 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }
        | '(' Exps ')'                  { Tuple $2 }     
        | '(' Exp ')'                   { Paren $2 }
        | Exp '.' num                   { Proj $1 $3 }

Exps    : Exp                           { [$1] }
        | Exp ',' Exps                  { $1 : $3 }

Type    : Boolean                       { TBool }
        | Number                        { TNum }
        | '(' Type "->" Type ')'        { TFun $2 $4 }
        | '(' Types ')'                 { TTuple $2 }     

Types   : Type                          { [$1] }
        | Type ',' Types                { $1 : $3 }

{ 
parseError :: [Token] -> a 
parseError _ = error "Erro sintático!"
}