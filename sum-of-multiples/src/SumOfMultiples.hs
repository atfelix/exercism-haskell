module SumOfMultiples (sumOfMultiples) where

import Data.List (any, filter, sum)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (anyFactorDivides factors) potentialMultiples
    where potentialMultiples = [1..limit - 1]

anyFactorDivides :: [Integer] -> Integer -> Bool
anyFactorDivides xs n = any (\ k -> k /= 0 && n `mod` k == 0) xs
