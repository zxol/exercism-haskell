{-# LANGUAGE TupleSections #-}

module ETL (transform) where

import Data.Char
import Data.Map (Map, fromList, foldrWithKey)

transform :: Map a String -> Map Char a
transform = fromList . foldrWithKey go []
  where
    go k v xs = map ((,k) . toLower) v <> xs
