module Codewars.NoZeros where

{-
    Numbers ending with zeros are boring.
    They might be fun in your world, but not here.
    Get rid of them. Only the ending ones.

    1450 -> 145
    960000 -> 96
    1050 -> 105
    -1050 -> -105
    Zero alone is fine, don't worry about it. Poor guy anyway
-}

noBoringZeros :: Int -> Int
noBoringZeros n
  | n == 0    = 0
  | r == 0    = noBoringZeros d
  | otherwise = n
  where (d,r) = n `divMod` 10

