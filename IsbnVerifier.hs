module IsbnVerifier (isbn) where

import Data.Char

isbn :: String -> Bool
isbn xs = let ys = filter (/= '-') xs; cd = last xs in
  (length ys == 10 && all isDigit (init ys) && (cd == 'X' || isDigit cd))
  && sum (zipWith ((*) . decode) ys [10,9..]) `mod` 11 == 0
  where decode c = if c == 'X' then 10 else digitToInt c
