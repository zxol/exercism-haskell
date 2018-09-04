module Change (findFewestCoins) where

import Data.List
import Control.Monad
import Data.Maybe
import Data.Array

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

emptyRoseTree val = RoseTree val []

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins n coins = undefined

ff :: Integer -> [Integer] -> RoseTree Integer -> RoseTree Integer
ff target [] tree = tree
ff 0 _ tree = tree
ff target options tree | target <= 0 = tree
ff target options tree = RoseTree target (map (\o -> ff (target - o) options tree ) options)

shortest :: RoseTree Integer -> Integer
shortest (RoseTree val []) = 0
shortest (RoseTree val (x:[])) = 0
shortest (RoseTree val (x:xs)) | left < rest = 1 + left
  where left = shortest x
        rest = shortest $ RoseTree val xs

qs = RoseTree 100 [RoseTree 50 [RoseTree 0 []], RoseTree 75 [RoseTree 50 [RoseTree 25 [RoseTree 0 []]]]]

takeCoinDP :: Integer -> [Integer] -> Maybe (Integer, [Integer])
takeCoinDP cents coins = get ltCoin cents
    where arr = array ((0,0), (ltCoin, cents)) [((i,c), takeC i c) | i <- [0..ltCoin], c <- [0..cents]]
          get i c
            | c < 0 || i < 0  = Nothing
            | c == 0          = Just (0, [])
            | otherwise       = arr!(i,c)
          ltCoin = length coins - 1
          takeC cNr cts
            | coin > cts          = get (cNr-1) cts
            | otherwise           = case (get cNr (cts-coin), get (cNr-1) cts) of
                                       (Just (n, t), Just (n',t')) -> Just $ if n+1 <= n' then (n+1, coin:t) else (n', t')
                                       (Nothing,     Just (n',t')) -> Just (n', t')
                                       (Just (n,t),  Nothing)      -> Just (n+1, coin:t)
                                       (Nothing,     Nothing)      -> Nothing
            where coin = coins !! cNr
