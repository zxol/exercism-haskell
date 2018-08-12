module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples gs n = sum . nub $ multiples n =<< gs

multiples :: Integer -> Integer -> [Integer]
multiples n g = takeWhile (< n) $ iterate (+g) 0
