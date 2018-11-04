module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List ((\\))

isPangram :: String -> Bool
isPangram text = null . (['a' .. 'z'] \\) $ map toLower text
