module Isogram (isIsogram) where
import Data.List
import Data.Char

isIsogram :: String -> Bool
isIsogram = all ((<= 1) . length) . group . sort . filter isLetter . map toLower
