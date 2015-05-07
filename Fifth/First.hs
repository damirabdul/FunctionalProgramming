module First where

data BinarySearchTree = Nil | Cons Int BinarySearchTree BinarySearchTree deriving(Show)
-- interp. binary search tree

empty_tree :: BinarySearchTree
empty_tree = Nil

one_elem_tree :: BinarySearchTree
one_elem_tree = Cons 1 empty_tree empty_tree

two_elem_tree :: BinarySearchTree
two_elem_tree = Cons 1 empty_tree (Cons 2 empty_tree empty_tree)

fn_for_tree :: BinarySearchTree -> a
fn_for_tree tree = case tree of
		Nil -> undefined
		Cons value left right -> undefined value (fn_for_tree left) (fn_for_tree right)

tree_height :: BinarySearchTree -> Int
tree_height tree = case tree of
	Nil -> 0
	Cons v left right -> (+) 1 (max (tree_height left) (tree_height right))

tree_sum :: BinarySearchTree -> Int
tree_sum tree = case tree of 
	Nil -> 0
	Cons v left right -> v + (tree_sum left) + (tree_sum right)

tree_search :: BinarySearchTree -> Int -> Bool
tree_search tree value = case tree of 
	Nil -> False
	Cons v left right -> if v == value
							then True
						else if value < v
								then tree_search left value
							else tree_search right value