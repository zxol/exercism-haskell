module SecretHandshake (handshake) where

import Data.Char
import Numeric

handshake :: Int -> [String]
handshake n = let bs = toBinaryString n in
  case head bs of
       '0' -> reverse $ actionList bs
       _ -> actionList bs

actionList :: String -> [String]
actionList bs = filter (/= "") $ zipWith go
                (tail bs)
                ["jump", "close your eyes", "double blink", "wink"]
                where
                  go '1' action = action
                  go _ _ = ""

toBinaryString :: Int -> String
toBinaryString n = lpad 5 $ showIntAtBase 2 intToDigit n ""

lpad :: Int -> String -> String
lpad m xs = replicate (m - length ys) '0' ++ ys
    where ys = take m xs

