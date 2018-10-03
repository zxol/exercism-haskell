module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond v | isUpper v = Just $ mirrorAppend $ foldr go [] ['A'..v]
          | otherwise = Nothing
  where
    len = index v
    mirrorAppend xs = xs <> reverse (init xs)
    go 'A' xs = (pad <> "A" <> pad) : xs
      where pad = replicate len ' '
    go x xs = (side <> [x] <> mid <> [x] <> side) : xs
      where
        side = replicate (len - index x) ' '
        mid = let w = 2 * index x - 1 in replicate w ' '

index :: Char -> Int
index = subtract 65 . fromEnum

