module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep $ not . p

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = if p x then x : keepRest else keepRest
    where keepRest = keep p xs
