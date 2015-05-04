module First where

type OatmealTemp = Int
-- interp. the oatmeal temperature. temperature could be in [0..20]

low_temp, preferenced_temp, high_temp :: OatmealTemp

low_temp = 0
preferenced_temp = 10
high_temp = 20

fn_for_oatmealTemp :: OatmealTemp -> a
fn_for_oatmealTemp t | 0 <= t && t <= 20 = undefined t


data Adjustment = ToLeft | ToRight | Current deriving (Show,Eq)
-- interp. the stove adjustment

fn_for_adjustment :: Adjustment -> a
fn_for_adjustment ad = case ad of
								  ToLeft -> undefined
								  ToRight -> undefined
								  Current -> undefined


oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment temp = if 0 <= temp && temp <= 20 
										then if temp > preferenced_temp 
												then ToLeft
											 else if temp < preferenced_temp
											 	then ToRight
											 	else Current
										else error "Wrong temperature"

