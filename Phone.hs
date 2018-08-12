module Phone (number) where

import Data.Char

number :: String -> Maybe String
number = vCheckPosition 0 . vCheckPosition 3 . vCountryCode . digitsOnly

digitsOnly :: String -> Maybe String
digitsOnly = Just . filter isNumber

vCountryCode :: Maybe String -> Maybe String
vCountryCode Nothing = Nothing
vCountryCode ( Just n )  | ( length n == 11 ) && head n == '1' = Just n
                         | length n == 10 = Just n
                         | otherwise = Nothing

type Position = Int

vCheckPosition :: Position -> Maybe String -> Maybe String
vCheckPosition _ Nothing = Nothing
vCheckPosition pos (Just n) | (length n == 11) && (twoToNine (n!!(succ pos))) = Just n
                       | (length n == 10) && twoToNine (n!!pos) = Just n
                       | otherwise = Nothing

twoToNine = (`elem` ['2'..'9'])
