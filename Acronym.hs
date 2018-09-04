module Acronym (abbreviate) where

import Data.Char

sanitize :: String -> String
sanitize = foldr go " "
  where
    go x xs =
      if isUpper (head xs) && isLower v
      then v:' ':xs -- insert space between camelCase
      else v:xs
      where
        v = if isLetter x then x else ' '

abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . sanitize
