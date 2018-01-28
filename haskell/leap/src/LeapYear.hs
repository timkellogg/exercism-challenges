module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear y =
  if y `mod` 4 == 0 && not (y `mod` 100 == 0)
    then True
    else if y `mod` 400 == 0
      then True
      else
        False

