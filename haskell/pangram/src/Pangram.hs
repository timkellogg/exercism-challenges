module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)

isPangram :: String -> Bool
isPangram xs = length ( filter (`elem` s) ['a'..'z'] ) == 26
  where s = map toLower ( filter isAlpha xs )
