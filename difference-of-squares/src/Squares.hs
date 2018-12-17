module Squares (difference, squareOfSum, sumOfSquares) where

import Control.Monad (ap, liftM2, join)

difference :: Integral a => a -> a
difference = liftM2 (-) squareOfSum sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum = square . (`div` 2) . ap (*) (1 +)

sumOfSquares :: Integral a => a -> a
sumOfSquares = (`div` 6) . liftM2 (*) (1 +) ((1 +) . (2 *) >>= (*))

square :: Integral a => a -> a
square = join (*)