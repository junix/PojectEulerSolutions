module Thinking.Chap02.H where

type CIN = [Char]
hello = 1


getDigit :: Char -> Int
getDigit c = read [c]

sum1 :: [Char] -> [Char]
sum1 cin =
    let
      s = show $ sum $ map getDigit cin
    in
      case s of
        [_] -> '0' : s
        _ -> s

addSum :: String -> String
addSum s = s ++
    case show $ sum $ map getDigit s of
      m@[_] -> '0' : m
      m -> m

valid :: String -> Bool
valid s
  | length s /= 8 = False
  | all isDigit s =
     let prefix = take 6 s
         sumPrefix = sum $ map getDigit prefix
         suffix = read (drop 6 s) :: Int
     in
         suffix == sumPrefix
  | otherwise = False
  where
    isDigit :: Char -> Bool
    isDigit x = x >= '0' && x <= '9'
