module PerfectNumbers (classify, Classification(..)) where

import Data.List

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n = case (n > 0, divisorSum n `compare` (2 * n)) of
    (False, _) -> Nothing
    (True, LT) -> Just Deficient
    (True, EQ) -> Just Perfect
    (True, GT) -> Just Abundant

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divisorSum :: Int -> Int
divisorSum n = Data.List.foldl' (\acc x -> acc + sum (divisorPair n x)) 0 $ divisors n

divisors :: Int -> [Int]
divisors n = Data.List.filter (\x -> n `mod` x == 0) [1..(isqrt n)]

divisorPair :: Int -> Int -> [Int]
divisorPair n d
    | (n `mod` d) == 0 = Data.List.nub [d, n `div` d]
    | otherwise = []