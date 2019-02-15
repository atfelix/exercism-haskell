{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Monad (mapM)
import Data.Bifunctor (bimap)
import Data.Map (Map, fromListWith)
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Bounded, Enum, Eq, Ord, Read, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Integer)
nucleotideCounts genome = fromListWith (+) <$> genomeTuples
    where genomeTuples = (map (,0) [minBound .. ] ++) <$> mapM eitherGenomeWithOne genome

eitherGenomeWithOne :: Char -> Either String (Nucleotide, Integer)
eitherGenomeWithOne = bimap (const "Invalid DNA symbol") (,1) . readEither . return