module SumOfMultiples (sumOfMultiples) where

import Data.List
import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples gs n = sum [x | x <- [1..n-1],  any (\y -> x `mod` y == 0 ) gs]

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

sumOfMultiplesNubOrd :: [Integer] -> Integer -> Integer
sumOfMultiplesNubOrd gs n = sum . nubOrd $ multiples' n =<< gs

sumOfMultiplesNub :: [Integer] -> Integer -> Integer
sumOfMultiplesNub gs n = sum . nub $ multiples' n =<< gs

multiples' :: Integer -> Integer -> [Integer]
multiples' n g = takeWhile (< n) $ iterate (+g) 0
