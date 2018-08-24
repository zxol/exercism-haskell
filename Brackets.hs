module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go ('[':xs) (']':ys) = go xs ys
    go ('{':xs) ('}':ys) = go xs ys
    go ('(':xs) (')':ys) = go xs ys
    go xs (y:ys)
      | y `elem` "[{(" = go (y:xs) ys
      | otherwise      = y `notElem` "]})" && go xs ys
    go xs [] = null xs
