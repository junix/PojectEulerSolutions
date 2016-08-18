module Euler.P179 where

import Math.NumberTheory.Primes
import qualified Data.Set as S

{-
Find the number of integers 1 < n < 107, for which n and n + 1 have the same number of positive divisors.
For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
-}

ds = map (S.size.divisors) [1..10^7-1]

euler = go ds
    where go [x] = 0
          go (x:y:xs)
             | x == y = 1 + go (y:xs)
             | otherwise = go (y:xs)
