module Interpreter where 

import Lexer 
import Data.List (findIndex)  

isValue :: Expr -> Bool 
isValue BTrue       = True 
isValue BFalse      = True  
isValue (Num _)     = True 
isValue (Lam _ _ _) = True
isValue (Tuple es)  = all isValue es  
isValue _           = False 

subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue 
subst v e BFalse = BFalse 
subst v e (Eq e1 e2) = Eq (subst v e e1) (subst v e e2)
subst v e (Lt e1 e2) = Lt (subst v e e1) (subst v e e2)
subst v e (Num x) = Num x 
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Mul e1 e2) = Mul (subst v e e1) (subst v e e2) 
subst v e (Div e1 e2) = Div (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (Or e1 e2) = Or (subst v e e1) (subst v e e2)
subst v e (Not e1) = Not (subst v e e1)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x) = if v == x then e else Var x 
subst v e (Lam x t b) = Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1) = Paren (subst v e e1)
subst v e (Tuple es) = Tuple (map (subst v e) es)  
subst v e (Proj e1 i) = Proj (subst v e e1) i     

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e) = Not (step e)
step (Mul (Num n1) (Num n2)) = Num (n1 * n2)   
step (Mul (Num n1) e2) = Mul (Num n1) (step e2)
step (Mul e1 e2) = Mul (step e1) e2
step (Div (Num n1) (Num n2)) = Num (n1 `div` n2)  
step (Div (Num n1) e2) = Div (Num n1) (step e2)
step (Div e1 e2) = Div (step e1) e2
step (And BTrue e2) = e2 
step (And BFalse e2) = BFalse 
step (And e1 e2) = And (step e1) e2 
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2 
step (Eq (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Eq BTrue BTrue) = BTrue
step (Eq BFalse BFalse) = BTrue
step (Eq BTrue BFalse) = BFalse
step (Eq BFalse BTrue) = BFalse
step (Eq e1 e2)
  | isValue e1 = Eq e1 (step e2)
  | otherwise = Eq (step e1) e2
step (Lt (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Lt e1 e2)
  | isValue e1 = Lt e1 (step e2)
  | otherwise = Lt (step e1) e2
step (App (Lam x t b) e2) | isValue e2 = subst x e2 b
                          | otherwise = App (Lam x t b) (step e2)
step (App e1 e2) = App (step e1) e2 
step (Paren e) = e 
step (Tuple es) =
    let (h, t) = span isValue es
    in case t of
        [] -> error "step was called on a value tuple, this should not happen"
        (e:es') -> Tuple (h ++ (step e : es'))

step (Proj (Tuple es) n) 
    | not (all isValue es) = Proj (step (Tuple es)) n 
    | n > 0 && n <= length es = es !! (n-1)
    | otherwise = error "indice invalido"
step (Proj e n) = Proj (step e) n

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)