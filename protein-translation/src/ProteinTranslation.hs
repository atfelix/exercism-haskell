module ProteinTranslation(proteins) where

import Control.Monad (sequence)
import Data.List (takeWhile, map)
import Data.List.Split (chunksOf)

proteins :: String -> Maybe [String]
proteins rna = sequence $ takeWhile (/= Nothing) $ map proteinFromCodon $ chunksOf 3 rna

proteinFromCodon :: String -> Maybe String
proteinFromCodon codon = case codon of
    "AUG" -> Just "Methionine"
    "UUU" -> Just "Phenylalanine"
    "UUC" -> Just "Phenylalanine"
    "UUA" -> Just "Leucine"
    "UUG" -> Just "Leucine"
    "UCU" -> Just "Serine"
    "UCC" -> Just "Serine"
    "UCA" -> Just "Serine"
    "UCG" -> Just "Serine"
    "UAU" -> Just "Tyrosine"
    "UAC" -> Just "Tyrosine"
    "UGU" -> Just "Cysteine"
    "UGC" -> Just "Cysteine"
    "UGG" -> Just "Tryptophan"
    "UAA" -> Nothing
    "UAG" -> Nothing
    "UGA" -> Nothing
    _ -> error "Invalid codon"
