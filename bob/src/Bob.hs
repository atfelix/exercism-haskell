module Bob (responseFor) where

import Data.Char(isUpper, isAlpha, isSpace)
import Data.List(dropWhileEnd)

responseFor :: String -> String
responseFor string
      | isWhiteSpace strippedString = "Fine. Be that way!"
      | isUppercase && isQ = "Calm down, I know what I'm doing!"
      | isUppercase = "Whoa, chill out!"
      | isQ = "Sure."
      | otherwise = "Whatever."
      where strippedString = dropWhileEnd isSpace string
            filteredString = filter isAlpha string
            isUppercase = isUppercaseString filteredString
            isQ = isQuestion strippedString

isUppercaseString :: String -> Bool
isUppercaseString xs = and $ [not . null, all isUpper] <*> [xs]

isQuestion :: String -> Bool
isQuestion xs = not (null xs) && last xs == '?'

isWhiteSpace :: String -> Bool
isWhiteSpace = all isSpace