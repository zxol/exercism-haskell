module Pangram (isPangram) where

import Data.Char

isPangram :: String ->  Bool
isPangram xs = all (`elem` map toLower xs) ['a'..'z']
