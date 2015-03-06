module Ackermann where

	ackermann :: Integer -> Integer -> Integer
	ackermann m n = if (m < 0) || (n < 0)
					then error "Digits must be positive"
					else if m == 0 
							then n + 1
							else if n == 0
									then ackermann (m-1) 1
									else ackermann (m - 1) (ackermann m (n - 1))
