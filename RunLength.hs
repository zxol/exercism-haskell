module RunLength (decode, encode) where

import Data.List
import Data.Char
import Text.Read

data Token = Single Char | Multiple Int Char
  deriving (Eq, Show)

-- ENCODE section

encode :: String -> String
encode = encodeTokens . map makeTokenEncode . group

makeTokenEncode :: String -> Token
makeTokenEncode xs | length xs == 1 = Single ( head xs )
                   | otherwise = Multiple (length xs) (head xs)

encodeTokens :: [Token] -> String
encodeTokens = concatMap go
  where
    go (Single c) = [c]
    go (Multiple n c) = show n ++ [c]

-- DECODE section

decode :: String -> String
decode = concatMap decodeToken . decodeTokens

decodeToken :: Token -> String
decodeToken (Single c) = [c]
decodeToken (Multiple n c) = replicate n c

decodeTokens :: String -> [Token]
decodeTokens = go []
  where
    go :: [Token] -> String -> [Token]
    go tokens [] = tokens
    go tokens xs = go ( tokens ++ [ makeTokenDecode xs ] ) ( rest xs )
    rest = tail . dropWhile isNumber

makeTokenDecode :: String -> Token
makeTokenDecode xs =
  let count = readMaybe (takeWhile isNumber xs)
      character = head (dropWhile isNumber xs)
  in case count of
      Nothing -> Single character
      Just n -> Multiple n character
