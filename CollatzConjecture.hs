module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n <= 0 = Nothing
collatz n = Just . toInteger . length . takeWhile (1 /=) $ iterate collatzNext n

collatzNext :: Integer -> Integer
collatzNext n
  | even n = n `div` 2
  | otherwise = 3 * n + 1
