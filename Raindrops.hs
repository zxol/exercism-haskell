module Raindrops (convert) where

convert :: Int -> String
convert n = let s = concatMap rain ( factors n )
  in if s == "" then show n else s

rain :: Int -> String
rain 3 = "Pling"
rain 5 = "Plang"
rain 7 = "Plong"
rain _ = ""

factors :: Int -> [Int]
factors n = filter ( (== 0) . mod n) [3,5,7]

