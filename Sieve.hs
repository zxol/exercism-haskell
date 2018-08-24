module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = go 2 [2..n]
  where
    go a xs | a == n = xs
    go a xs = go (succ a) (removeMultiples xs)
      where
        removeMultiples = filter (\v -> v == a || v `mod` a /= 0)

primesUpTo' :: Integer -> [Integer]
primesUpTo' n = go [2..n] [2..n]
  where
    go [] ys = ys
    go (x:xs) ys
      | notFactored = let nys = filter factorCheck ys in
          if length ys == length nys
          then ys
          else go xs $ filter factorCheck ys
      | otherwise = go xs ys
      where
        factorCheck a = a == x || a `mod` x /= 0
        notFactored = not $ any ( (== 0) . mod x) (takeWhile ((< x) . fromIntegral) ys)
