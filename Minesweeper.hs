module Minesweeper (annotate) where

import Control.Lens

type Board = [String]
type Point = (Int, Int)
type MaybeCell = Maybe Char

width :: Board -> Int
width [] = 0
width b = length $ head b

height :: Board -> Int
height = length

inRange :: Int -> Int -> Bool
inRange i x = x >= 0 && x < i

inWidth :: Board -> Int -> Bool
inWidth = inRange . width

inHeight :: Board -> Int -> Bool
inHeight = inRange . height

getCell :: Board -> Point -> MaybeCell
getCell b (x, y) = if inWidth b x && inHeight b y
                   then Just (b !! y !! x)
                   else Nothing

surroundingOffsets :: [Point]
surroundingOffsets = [(-1, 1), (0, 1), (1, 1),
                      (-1, 0),         (1, 0),
                      (-1,-1), (0,-1), (1,-1)]

addP :: Point -> Point -> Point
addP (x, y) (x', y') = (x + x', y + y')

neighbours :: Board -> Point -> [MaybeCell]
neighbours b p = map (getCell b . addP p) surroundingOffsets

countTrue :: [Bool] -> Int
countTrue = length . filter id

hasMine :: MaybeCell -> Bool
hasMine (Just c) = c == '*'
hasMine Nothing = False

noZero :: Char -> Char
noZero c = if c == '0' then ' ' else c

mineCount :: Board -> Point -> Char
mineCount b p = noZero . head . show . countTrue
                . map hasMine $ neighbours b p

setCell :: Board -> Point -> Char -> Board
setCell b (x, y) c = b & ix y . ix x .~ c

listPointsOnBoard :: Board -> [Point]
listPointsOnBoard b = [(x,y) | x <- [0..width b], y <- [0..height b]]

annotate :: Board -> Board
annotate board = foldr go board $ listPointsOnBoard board
  where
    go p b | hasMine $ getCell b p = b
           | otherwise = setCell b p (mineCount b p)
