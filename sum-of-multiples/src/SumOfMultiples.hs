module SumOfMultiples (sumOfMultiples) where

import Data.List (any, filter, sum, takeWhile)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (anyFactorDivides factors) potentialMultiples
    where potentialMultiples = [1..limit - 1]

anyFactorDivides :: [Integer] -> Integer -> Bool
anyFactorDivides xs n = any ((== 0) . (n `mod`)) xs
