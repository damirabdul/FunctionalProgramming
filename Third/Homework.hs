module Homework where

	tailFibonacci :: Integer -> Integer

	tailFibonacci n
		| n == 0 = 0
		| otherwise = tailFib n 1 0
						where 
							tailFib 0 result previous = result
							tailFib n result previous = tailFib (n - 1) (result + previous) result



	binom :: Integer -> Integer -> Integer
	binom n k = binom' 1 1 n k
  		where
    		binom' r p n 0 = r `div` p
    		binom' r p 0 k = 0
    		binom' r p n k = binom' (r * n) (p * k) (n-1) (k-1)
