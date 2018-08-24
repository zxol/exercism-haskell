module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSums n - sumOfSquares n

-- using partial sum formulas (can process a 6000 digit n in 0.01 sec, but it's only an approximation. seems to be very accurate upto about 5 digit n)

squareOfSums :: Integral a => a -> a
squareOfSums m = let o = fromIntegral m in
  round (((1 / 2) * o * (o + 1)) ^ 2)

sumOfSquares :: Integral a => a -> a
sumOfSquares m = let o = fromIntegral m in
  round ((1 / 6) * o * (o + 1) * (2 * o + 1))

-- using recursion (takes 15 seconds to process an 8 digit n.  Not an approximation) 

difference' :: Integral a => a -> a
difference' n = squareOfSums' n - sumOfSquares' n

squareOfSums' :: Integral a => a -> a
squareOfSums' n = sum [1..n] ^ 2

sumOfSquares' :: Integral a => a -> a
sumOfSquares' n = sum $ map (^2) [1..n]
