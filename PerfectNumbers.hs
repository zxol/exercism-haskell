module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n > 0 = ( go . sum . factors ) n
           | otherwise = Nothing
    where
      go s | s > n = Just Abundant
           | s == n = Just Perfect
           | s < n = Just Deficient
           | otherwise = Nothing

factors :: Int -> [Int]
factors n = filter ((== 0) . mod n) [1..(n-1)]
