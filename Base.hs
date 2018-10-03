module Base (Error(..), rebase) where

import Data.List (unfoldr, find)
import Data.Maybe (isJust, fromJust)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

type Digits = [Integer]
type Base = Integer

rebase :: Base -> Base -> Digits -> Either (Error Integer) Digits
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | isJust invalid = Left $ InvalidDigit $ fromJust invalid
  | otherwise = Right $ intToBase (toInt inputBase inputDigits) outputBase
  where
    invalid = find (\x -> x >= inputBase || x < 0) inputDigits

toInt :: Base -> Digits -> Integer
toInt base digits = sum $ zipWith go (reverse digits) [0..]
  where
    go d i = d * (base^i)

intToBase :: Integer -> Base -> Digits
intToBase i base = reverse $ unfoldr go i
  where
    go 0 = Nothing
    go n = Just (n `mod` base, n `div` base)
