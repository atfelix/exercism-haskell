module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Monad (mapM)
import Data.Map (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fromListWith (+) <$> ys
    where ys = (zs ++) <$> mapM f xs
          zs = [(A, 0), (C, 0), (G, 0), (T, 0)]

f :: Char -> Either String (Nucleotide, Int)
f x = case x of
    'A' -> Right (A, 1)
    'C' -> Right (C, 1)
    'G' -> Right (G, 1)
    'T' -> Right (T, 1)
    _ -> Left "Invalid DNA symbol"