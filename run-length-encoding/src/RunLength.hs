module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, replicate, tail, span, concatMap)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)

type Pair = (Int, Maybe Char)

decode :: String -> String
decode s = concatMap pairToDecodedString $ stringToDecodedPairs s

stringToDecodedPairs :: String -> [Pair]
stringToDecodedPairs "" = []
stringToDecodedPairs s = stringPairToDecodedString (x, y) : stringToDecodedPairs (tail y)
    where (x, y) = span isDigit s

stringPairToDecodedString :: (String, String) -> Pair
stringPairToDecodedString (_, "") = (0, Nothing)
stringPairToDecodedString (maybeInt, s) = (fromMaybe 1 $ readMaybe maybeInt, listToMaybe s)

pairToDecodedString :: Pair -> String
pairToDecodedString (n, Just x) = replicate n x

encode :: String -> String
encode text = concatMap (pairToString . stringToEncodedPair) $ group text

stringToEncodedPair :: String -> Pair
stringToEncodedPair s = (length s, listToMaybe s)

pairToString :: Pair -> String
pairToString (0, _) = ""
pairToString (_, Nothing) = ""
pairToString (n, Just x) = if n == 1 then [x] else show n ++ [x]