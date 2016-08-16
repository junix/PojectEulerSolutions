module Euler.P132 where
import Math.NumberTheory.Primes

modPow b 0 m = 1
modPow b 1 m = b `rem` m
modPow b e m = (v*v*v') `rem` m
    where (q, r) = e `quotRem` 2
          v      = modPow b q m
          v'     = modPow b r m

euler = sum . take 40 .
        filter ((==1).modPow 10 (10^10).(*9)) $
        primes
