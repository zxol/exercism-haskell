module Grains (square, total) where

import Data.Maybe
import Control.Monad

toMaybe :: Bool -> a -> Maybe a
toMaybe b a = if b then Just a else Nothing

boardRange :: Integer -> Bool
boardRange = liftM2 (&&) (> 0) (<= 64)

square :: Integer -> Maybe Integer
square = (toMaybe . boardRange) <*> (2 ^) . subtract 1

total :: Integer
total = fromMaybe 0 $ sum <$> mapM square [1..64]

squareSeq :: Int -> [Integer]
squareSeq = flip take ( iterate (*2) 1 )

