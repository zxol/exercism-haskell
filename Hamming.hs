module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs == length ys =
                 Just . sum $ zipWith comparePoint xs ys
               | otherwise = Nothing

comparePoint :: Char -> Char -> Int
comparePoint a b | a == b = 0
                 | otherwise = 1
