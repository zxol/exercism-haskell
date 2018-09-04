module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n $ iterate nextRow [1]

nextRow :: [Integer] -> [Integer]
nextRow v = zipWith (+) (v++[0]) ([0]++v)
