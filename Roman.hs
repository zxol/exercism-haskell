module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n | n > 0 && n <= 3000 = Just $ concat $ zipWith (!!) numeralMatrix $ digits n
           | otherwise = Nothing

digits :: Integer -> [Int]
digits = lpad . map (read . (:[])) . show
  where
    lpad xs | length xs < 4 = replicate (4 - length xs) 0 <> xs
            | otherwise = take 4 xs

numeralMatrix :: [[String]]
numeralMatrix = reverse [makeRow 'I' 'V' 'X',
                         makeRow 'X' 'L' 'C',
                         makeRow 'C' 'D' 'M',
                         makeRow 'M' '?' '?']
  where
    makeRow a b c = ["", [a], [a,a], [a,a,a], [a,b], [b], [b,a], [b,a,a], [b,a,a,a], [a,c]]
