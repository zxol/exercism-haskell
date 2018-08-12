module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

type Triplet = (Int, Int, Int)

isPythagorean :: Triplet -> Bool
isPythagorean (a,b,c) = a*a+b*b==c*c || a*a+c*c==b*b || b*b+c*c==a*a

mkTriplet :: Int -> Int -> Int -> Triplet
mkTriplet a b c = (a,b,c)

pythagoreanTriplets :: Int -> Int -> [Triplet]
pythagoreanTriplets m x = [ mkTriplet a b c | a <- [m..x], b <- [a..x], c <- [b..x], isPythagorean (a,b,c)]
