module Scrabble (scoreLetter, scoreWord) where

import Data.Char

scoreLetter :: Char -> Integer
scoreLetter = go . toUpper
  where
    go l
      | l `elem` ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'] = 1
      | l `elem` ['D', 'G'] = 2
      | l `elem` ['B', 'C', 'M', 'P'] = 3
      | l `elem` ['F', 'H', 'V', 'W', 'Y'] = 4
      | l `elem` ['K'] = 5
      | l `elem` ['J', 'X'] = 8
      | l `elem` ['Q', 'Z'] = 10
      | otherwise = 0

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
