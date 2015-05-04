module Second where

data DinnerOrder = Chicken | Pasta | NoFood deriving (Show,Eq)
-- interp. dinner options

fn_for_dinner_order :: DinnerOrder -> a
fn_for_dinner_order d = case d of
							Chicken -> undefined
							Pasta -> undefined
							NoFood -> undefined

dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg d = case d of
							Chicken -> "The passenger ordered chicken."
							Pasta -> "The passenger ordered pasta."
							NoFood -> "The passenger ordered nothing."