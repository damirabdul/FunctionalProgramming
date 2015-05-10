module First where

map_via_foldl :: (a -> b) -> [a] -> [b]
map_via_foldl fn ar = foldl(\list el -> list ++ [(fn el)]) [] ar

map_via_foldr :: (a -> b) -> [a] -> [b]
map_via_foldr fn ar = foldr (\el list -> [(fn el)] ++ list) [] ar