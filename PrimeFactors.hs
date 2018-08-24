-- PrimeFactors.hs
-- This function takes a number and generates a list of prime numbers whose product is the given number. For example, the list for the number 60 is 2 x 2 x 3 x 5 = 60 [2,2,3,5]

module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = flip go [2]
  where
    go :: Integer -> [Integer] -> [Integer]
    go 1 fs = reverse $ tail fs
    go t fs | t `mod` curr == 0 = go (t `div` curr) (curr:fs)
            | otherwise = go t ((curr + 1):(tail fs))
            where curr = head fs
