module Codewars.Arrays where

{-
You get an array of numbers, return the sum of all of the positives ones.
Example [1,-4,7,12] => 1 + 7 + 12 = 20
-}

positiveSum :: [Int] -> Int
positiveSum = sum.filter (>0)


