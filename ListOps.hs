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
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldr (const (+1)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)

concat :: [[a]] -> [a]
concat = foldr (++) []
