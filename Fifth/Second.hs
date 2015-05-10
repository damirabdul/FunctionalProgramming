module Second where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving(Show)
--interp. tree that contains elems in leafs of some type

one_elem_tree :: Tree Int
one_elem_tree = Leaf 1

two_elem_tree :: Tree String
two_elem_tree = Branch (Leaf "hello") (Leaf "world")

fn_for_tree :: Tree a -> b
fn_for_tree tree = case tree of
				Leaf a -> undefined a
				Branch left right -> (fn_for_tree left) (fn_for_tree right)

tree_height :: Tree a -> Int
tree_height tree = case tree of
				Leaf a -> 1
				Branch left right -> 1 + max(tree_height left) (tree_height right)

tmap :: Tree a -> (a -> b) -> Tree b
tmap tree fn = case tree of
				Leaf a -> Leaf (fn a)
				Branch left right -> Branch (tmap left fn) (tmap right fn)




