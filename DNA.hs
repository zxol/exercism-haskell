module DNA (nucleotideCounts) where

import qualified Data.Map as M

addToMap :: Char -> M.Map Char Int -> M.Map Char Int
addToMap c = M.insertWith (+) c 1

isNuc :: Char -> Bool
isNuc = flip elem "GATC"

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts = foldr go ( Right $ M.fromList [('A',0),('C',0),('G',0),('T',0)] )
  where
    go _ (Left e) = Left e
    go c (Right m)
      | isNuc c = Right $ addToMap c m
      | otherwise = Left "Invalid Nucleotide"
