module Folding
  where

myfoldr f z [] = z
myfoldr f z (x:xs) = x `f` myfoldr f z xs

myfoldl f z [] = z
myfoldl f z (x:xs) = let z' = z `f` x in myfoldl f z' xs

myfoldrl f z xs = foldr go id xs z
  where
    go v acc x = acc (f x v)
