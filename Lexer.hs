module Lexer where 

import Data.Char

data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | And Expr Expr 
          | Or Expr Expr
          | Not Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Eq Expr Expr        
          | Lt Expr Expr     
          | If Expr Expr Expr 
          | Var String 
          | Lam String Ty Expr 
          | App Expr Expr 
          | Paren Expr 
          | Tuple [Expr]         
          | Proj Expr Int       
          deriving Show 

data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TTuple [Ty]          
        deriving (Show, Eq)


data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenAnd 
           | TokenOr
           | TokenMul   
           | TokenComma
           | TokenNot
           | TokenEq
           | TokenLt
           | TokenDiv
           | TokenIf 
           | TokenThen
           | TokenElse 
           | TokenVar String 
           | TokenLam 
           | TokenColon
           | TokenArrow 
           | TokenTNum 
           | TokenTBool
           | TokenLParen 
           | TokenRParen
           | TokenDot            
           deriving Show 

lexer :: String -> [Token]
lexer [] = [] 
lexer ('+':cs) = TokenAdd : lexer cs 
lexer ('\\':cs) = TokenLam : lexer cs 
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs 
lexer ('*':cs) = TokenMul : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs 
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':cs)     = TokenLt : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs) 
             | isAlpha c = lexKW (c:cs)

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("Number", rest) -> TokenTNum : lexer rest 
             ("Boolean", rest) -> TokenTBool : lexer rest 
             (var, rest) -> TokenVar var : lexer rest 