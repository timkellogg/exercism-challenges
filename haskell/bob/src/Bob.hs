module Bob (responseFor) where

import Data.Char (toUpper, isDigit, isSpace, isLetter)

responseFor :: String -> String
responseFor xs
  | isBlank xs                                              = "Fine. Be that way!"
  | hasNoLetters xs && not (isQuestion xs)                  = "Whatever."
  | isQuestion xs && isShouting xs && not (hasNoLetters xs) = "Calm down, I know what I'm doing!"
  | isQuestion xs                                           = "Sure."
  | isShouting xs                                           = "Whoa, chill out!"
  | otherwise                                               = "Whatever."

isQuestion :: String -> Bool
isQuestion xs = head ( reverse ( filter ( not . isSpace ) xs ) ) == '?'

isShouting :: String -> Bool
isShouting xs = map toUpper xs == xs

isBlank :: String -> Bool
isBlank xs = length ( filter ( not . isSpace ) xs ) == 0

isOnlyNumbers :: String -> Bool
isOnlyNumbers xs = all isDigit xs

hasNoLetters :: String -> Bool
hasNoLetters xs = length ( filter isLetter xs ) == 0
