module DNA (toRNA) where

import Control.Monad (mapM)

toRNA :: String -> Either Char String
toRNA = mapM toRNAChar

toRNAChar :: Char -> Either Char Char
toRNAChar char = case char of
    'A' -> Right 'U'
    'C' -> Right 'G'
    'G' -> Right 'C'
    'T' -> Right 'A'
    c -> Left c
    