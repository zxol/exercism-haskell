module RunLength (decode, encode) where

import Data.List
import Data.Char

data Token = Single Char | Multiple Int Char
  deriving (Eq, Show)

-- ENCODE section

encode :: String -> String
encode = concatMap go . group
  where
    go xs | length xs < 2 = xs
          | otherwise = show (length xs) <> [head xs]

-- DECODE section

decode :: String -> String
decode = concatMap go . seperate
  where
    go (c, "") = [c]
    go (c, num) = replicate (read num :: Int) c

seperate :: String -> [(Char, String)]
seperate = init . foldr go [(' ', "")]
  where
    go x xs@((c,digits):ys)
      | isDigit x = (c, x:digits):ys
      | otherwise = (x, ""):xs
