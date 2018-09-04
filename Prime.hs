module Prime (nth) where

import Data.List.Ordered (minus, union, unionAll)

nth :: Int -> Maybe Integer
nth n | n < 1 = Nothing
      | otherwise = Just $ primes !! (n-1)

-- taken from my sieve of erastophenes submission

primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])
