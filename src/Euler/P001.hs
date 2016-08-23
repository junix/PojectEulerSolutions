module Euler.P001 where
import Data.List(union)

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
-}

compOf35 n
    | n `rem` 3 == 0 = True
    | n `rem` 5 == 0 = True
    | otherwise     = False

seq35 = sum . filter compOf35 $ [3..999]

pe01 = sum (mseq 3 `union` mseq 5)
    where mseq n = takeWhile (<1000) . filter ((==0) . (`rem` n)) $ [0..]

pe02 = sum s3 + sum s5 - sum s15
    where s3  = [3,6..999]
          s5  = [5,10..999]
          s15 = [15,30..999]
