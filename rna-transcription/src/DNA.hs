module DNA (toRNA) where

import Control.Monad (mapM)

toRNA :: String -> Maybe String
toRNA = mapM toRNAChar

toRNAChar :: Char -> Maybe Char
toRNAChar char = case char of
    'A' -> Just 'U'
    'C' -> Just 'G'
    'G' -> Just 'C'
    'T' -> Just 'A'
    _ -> Nothing
    