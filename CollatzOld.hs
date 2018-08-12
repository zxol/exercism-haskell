module CollatzConjecture (collatz) where

import Data.List

collatz :: Integer -> Maybe Integer
collatz n | (n <= 0) = Nothing
collatz n = Just . toInteger . length . takeWhile (1 /=) $ iterate collatz' n

collatz' :: Integer -> Integer
collatz' n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzF :: Integer -> Maybe Integer
collatzF = collatz' 0
  where
    -- collatz' :: Integer -> Integer -> Maybe Integer
    collatz' i n
      | n <= 0 = Nothing
      | n == 1 = Just i
      | even n = collatz' (succ i) (n `div` 2)
      | otherwise = collatz' (succ i) (3 * n + 1)


-- I wanted to generate a list using unfoldr to have a look at the sequences.
-- I thought I'd include my code here as a footnote.
-- I think collatzCount is less economic than my first attempt.

collatzSeq :: Integer -> [Integer]
collatzSeq = unfoldr go
  where
    go n
      | n == 0 = Nothing
      | n == 1 = Just (1, 0)
      | even n = Just (n, n `div` 2)
      | otherwise = Just (n, 3 * n + 1)

collatzCount :: Integer -> Int
collatzCount = pred . length . collatzSeq

-- I will have to revisit this problem later when I can use N-Trees to make a map of collatz sequences
