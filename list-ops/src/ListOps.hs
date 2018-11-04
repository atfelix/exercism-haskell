module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = let acc' = f acc x in acc' `seq` foldl' f acc' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc xs = foldl' (flip f) acc $ reverse xs

length :: [a] -> Int
length = foldl' (const . (1+)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\ x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\ x acc -> if p x then x : acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldl' (++) []
