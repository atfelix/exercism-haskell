module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import Data.List ((\\))

isIsogram :: String -> Bool
isIsogram s = null (lower \\ ['a' .. 'z'])
    where lower = toLower <$> filter isAlpha s
