module RotationalCipher (rotate) where

import Data.Char

-- rShift: increment a letter to the right, cyclicly. a to b, b to c, z to a, etc...
-- preserves case and non alpha characters
rShift :: Char -> Char
rShift 'Z' = 'A'
rShift 'z' = 'a'
rShift c | isLetter c && isAscii c = succ c
rShift c = c

rotate :: Int -> String -> String
rotate n = map go
    where
        go c = iterate rShift c !! n

