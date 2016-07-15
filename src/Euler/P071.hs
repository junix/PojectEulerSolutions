module Euler.P071 where
import Data.Ratio
import Data.List (foldr)

best o@(ln,ld) n = go [u,(u-1)..d]
    where u = 3*n `quot` 7
          d = ln*n `quot` ld
          go [] = o
          go (x:xs)
            | 7*x >= 3*n  = go xs
            | n*ln>= ld*x = o
            | otherwise   = let r = x % n in (numerator r, denominator r)

euler = foldl best (2,5) [8..10^6]
euler' = maximum [ bestn d % d | d <- [8..10^6]]
    where bestn x = let (q,r) = (3*x) `quotRem` 7
                    in if r == 0
                       then q - 1
                       else q
