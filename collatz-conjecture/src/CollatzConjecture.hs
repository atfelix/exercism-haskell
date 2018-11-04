module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = case (n `compare` 1, n `mod` 2 == 0) of
    (LT, _) -> Nothing
    (EQ, _) -> Just 0
    (GT, True) -> (1+) <$> collatz (n `div` 2)
    (GT, False) -> (1+) <$> collatz (3 * n + 1)
    