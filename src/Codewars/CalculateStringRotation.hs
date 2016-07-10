{-# LANGUAGE UnicodeSyntax #-}
module Codewars.CalculateStringRotation where

{-
Description:

The goal of this exercise is to write a method that takes two strings as parameters and returns an integer n, where n is equal to the amount of spaces "rotated forward" the second string is relative to the first string (more precisely, to the first character of the first string).
For instance, take the strings "fatigue" and "tiguefa". In this case, the first string has been rotated 5 characters forward to produce the second string, so 5 would be returned.
If the second string isn't a valid rotation of the first string, the method returns -1.
Examples:

"coffee" , "eecoff" => 2
"eecoff" , "coffee" => 4
"moose"  , "Moose"  => -1
"isn't"  , "'tisn"  => 2
"Esham"  , "Esham"  => 0
"dog"    , "god"    => -1
-}


shiftedDiff :: String -> String -> Int
shiftedDiff "" "" = 0
shiftedDiff a b
    | length a /= length b = -1
shiftedDiff a b = go 0 a (cycle b)
    where lb = length b
          isPrefix xs ys = all (==True) $ zipWith (==) xs ys
          go :: Int -> String -> String -> Int
          go n xs ys@(y:ys')
            | n >= lb   = -1
            | otherwise = if isPrefix xs ys then n else go (n+1) xs ys'

isPrefix xs ys = all (==True) $ zipWith (==) xs ys

s a b = case matched of
          [] -> -1
          (index:_) -> index
    where
          cb = cycle b
          matched = dropWhile (\i -> not(isPrefix a (drop i cb)))  [0 .. length b]


