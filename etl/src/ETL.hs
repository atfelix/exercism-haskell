module ETL (transform) where

import Data.Char (toLower)
import Data.List (foldl)
import Data.Map.Strict (Map, fromList, assocs)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ foldl (\acc pair -> acc ++ transformToList pair) [] $ assocs legacyData

transformToList :: (a, String) -> [(Char, a)]
transformToList (x, s) = [(toLower char, x) | char <- s]