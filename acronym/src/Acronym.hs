module Acronym (abbreviate) where

import Data.Char (toUpper, isAlpha)
import Data.List (head)
import Data.List.Split (condense, dropDelims, split, startsWithOneOf, whenElt)

abbreviate :: String -> String
abbreviate xs = map (toUpper . head) $ concatMap splitOnUpper (splitOnNonAlpha xs)

splitOnUpper :: String -> [String]
splitOnUpper = split (condense $ startsWithOneOf ['A'..'Z'])

splitOnNonAlpha :: String -> [String]
splitOnNonAlpha = split (dropDelims . condense $ whenElt (not . isAlpha))
