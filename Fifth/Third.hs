module Third where

data MyList a b = Nil | Cons a (MyList b a) deriving (Show, Eq)
-- interp. list with different types of values

empty_list :: MyList a b
empty_list = Nil

two_elem_list :: MyList Int String
two_elem_list = Cons 5 (Cons "hello" empty_list)

fn_for_list :: MyList a b -> c
fn_for_list list = case list of 
	Nil -> undefined
	Cons a list -> undefined a (fn_for_list list)

list_length :: MyList a b -> Int
list_length list = case list of
						Nil -> 0
						Cons a tl -> (+) 1 (list_length tl)

dmap :: MyList a b -> (a -> c) -> (b -> d) -> MyList c d
dmap list fn1 fn2 = case list of 
						Nil -> Nil
						Cons a tl -> Cons (fn1 a) (dmap tl fn2 fn1)
	         