module Phone (number) where

import Data.Char

number :: String -> Maybe String
number = vCheckPosition 0 . vCheckPosition 3 . vCountryCode . digitsOnly

digitsOnly :: String -> String
digitsOnly = filter isNumber

vCountryCode :: String -> Maybe String
vCountryCode n | ( length n == 11 ) && head n == '1' = Just $ tail n
               | length n == 10 = Just n
               | otherwise = Nothing

vCheckPosition :: Int -> Maybe String -> Maybe String
vCheckPosition _ Nothing = Nothing
vCheckPosition pos (Just n) | (n!!pos) `elem` ['2'..'9'] = Just n
                            | otherwise = Nothing
