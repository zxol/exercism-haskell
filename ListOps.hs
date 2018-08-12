module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = go
  where
    go [] = z
    go (x:xs) = let z' = z `f` x in seq z' $ foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = go
  where
    go [] = z
    go (x:xs) = x `f` go xs

length :: [a] -> Int
length = foldr (const (+1)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr go []
  where
    go y ys | p y = y : ys
            | otherwise = ys

(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)

concat :: [[a]] -> [a]
concat = foldr (++) []
