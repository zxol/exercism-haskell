module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = error "You need to implement this function."

substrings :: Int -> String -> [String]
substrings s xs = map go xs
  where
    go x = undefined

sublists  _     0 = [[]]
sublists  []    _ = []
sublists (x:xs) n = sublists xs n ++ map (x:) (sublists xs $ n - 1)
