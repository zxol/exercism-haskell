module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f = filter (not . f)

keep :: (a -> Bool) -> [a] -> [a]
keep = filter
