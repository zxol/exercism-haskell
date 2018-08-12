module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: ( Ord a, Eq a, Num a ) => a -> a -> a -> TriangleType
triangleType a b c | any checkIllegal $ permutations [a,b,c] = Illegal
                   | otherwise = case length $ nub [a,b,c] of
                                    1 -> Equilateral
                                    2 -> Isosceles
                                    3 -> Scalene
                                    _ -> Illegal

checkIllegal :: (Eq a, Ord a, Num a) => [a] -> Bool
checkIllegal xs = (xs!!0 == 0) || ((xs!!0 + xs!!1) < xs!!2)
