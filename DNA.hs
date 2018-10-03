module DNA (nucleotideCounts) where

import qualified Data.Map as M

incrementMapItem :: Char -> M.Map Char Int -> M.Map Char Int
incrementMapItem c = M.insertWith (+) c 1

nucleotidesList :: Either String (M.Map Char Int)
nucleotidesList = Right $ M.fromList $ zip "GATC" $ repeat 0

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts = foldr go nucleotidesList
  where
    go _ (Left e) = Left e
    go c (Right m)
      | isNuc c = Right $ incrementMapItem c m
      | otherwise = Left "Invalid Nucleotide"
    isNuc = flip elem "GATC"
