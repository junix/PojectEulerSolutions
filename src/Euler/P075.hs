module Euler.P075 where
import Math.NumberTheory.Primes
import Data.List (group, sort, nub)

limit = 1500000
maxm = truncate . sqrt . fromInteger $ (limit `quot` 2)

genSet :: [Integer]
genSet = concat [ gen l l | m <- [2..maxm]
                          , n <- [1..m-1]
                          , gcd m n == 1
                          , odd (m+n)
                          , let l = 2*m*(m + n) ]
  where gen l c
          | c > limit = []
          | otherwise = c : gen l (l+c)

euler = length . filter ((==1).length) . group . sort $ genSet
