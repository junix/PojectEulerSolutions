module Euler.P001 where

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
-}

compOf35 n
    | n `rem` 3 == 0 = True
    | n `rem` 5 == 0 = True
    | otherwise     = False

seq35 = sum . filter compOf35 $ [3..999]
