module Euler.P130 where
import Math.NumberTheory.Primes

r 1 = 1
r n = r (n-1) * 10 + 1

a n = go 1 1
    where go x c = if x `rem` n == 0
                     then c
                     else go (x*10+1) (c+1)

pseq = filter (\n -> (n-1) `rem` a n == 0) .
       filter (not.isPrime)                .
       filter (\x -> gcd 10 x == 1)        $
       [2..]

euler = sum . take 25 $ pseq