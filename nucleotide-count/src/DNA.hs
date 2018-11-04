module DNA (nucleotideCounts) where

import Control.Monad (mapM)
import Data.Map (fromListWith)

nucleotideCounts xs = fromListWith (+) <$> ys
    where ys = (zs ++) <$> mapM f xs
          zs = [('A', 0), ('C', 0), ('G', 0), ('T', 0)]

f :: Char -> Either String (Char, Int)
f x = case x of
    'A' -> Right ('A', 1)
    'C' -> Right ('C', 1)
    'G' -> Right ('G', 1)
    'T' -> Right ('T', 1)
    _ -> Left "Invalid DNA symbol"