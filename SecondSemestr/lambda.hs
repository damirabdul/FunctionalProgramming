module Lambda where

import Data.Maybe (fromJust)
 
data Type = IntType | ArrowType Type Type deriving (Show, Eq)
-- structure of Type data

fn_for_type :: Type -> a
fn_for_type t = case t of 
						  IntType -> undefined
						  ArrowType t1 t2 -> undefined t1 t2
 
data Term = Const Int | Var String | Abs String Type Term | App Term Term deriving (Show, Eq)
-- structure of Term data

fn_for_term :: Term -> a
fn_for_term t = case t of 
						  Const n -> undefined
						  Var str -> undefined str
						  Abs str typ ter -> undefined str typ ter
						  App term1 term2 -> undefined term1 term2
 
typeof :: [(String, Type)] -> Term -> Type
typeof _ Const{} = IntType
typeof e (Var x) = fromJust (lookup x e)
typeof e (Abs arg ty t) = ArrowType ty (typeof ((arg, ty):e) t)
typeof e (App t arg) =
	let ArrowType ty ret = typeof e t in
	if typeof e arg /= ty then
		error "argument type mismatch"
	else ret
 
replace :: String -> Term -> Term -> Term
replace str t1 (Var v') | v' == str = t1
replace str t1 (Abs s ty ter) = Abs s ty (replace str t1 ter)
replace str t1 (App term1 term2) = App (replace str t1 term1) term2
replace _ _ t2 = t2
 
eval :: [(String, Term)] -> Term -> Term
eval _ (Const n) = Const n
eval _ Abs{} = Abs{}
eval e (Var str) = Const (let Const n = fromJust (lookup str e) in n)
eval e (App (Abs str _ ter) term2) = replace str term2 ter
eval e (App term1 term2) = eval e (App (eval e term1) term2)
 
main = do
	let first = App (Abs "x" IntType (Var "x")) (Const 42)
	putStrLn $ show(eval [] first)
	let second = App (App (Abs "x" IntType $ Abs "y" IntType (Var "x")) (Const 42)) (Const 12)
	putStrLn $ show(eval [] second)