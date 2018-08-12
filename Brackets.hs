module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = error "You need to implement this function."

parser :: [String] -> [String] -- using array maybe
parser [] = []
parser (x:_) = go x
  where
    g (y:ys) | bracketDir y = 

bracketDir :: Char -> Bool
bracketDir c = c `elem` "{[("
