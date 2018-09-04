module School (School, add, empty, grade, sorted) where

import Data.List
import Data.Maybe
import Data.Function
import Control.Lens
import qualified Data.Map as M

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school = school & at gradeNum <>~ Just [student]

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade gradeNum school = fromMaybe [] $ school ^. at gradeNum

sorted :: School -> [(Int, [String])]
sorted = M.toList . M.map sort
