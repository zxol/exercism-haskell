module Acronym (abbreviate) where

import Data.Char

sanitize :: String -> String
sanitize = foldr go " "
  where
    go x a = let v = if isLetter x then x else ' ' in
      if isUpper (head a) && isLower v
      then v:' ':a -- insert space between camelCase
      else v:a

abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . sanitize
