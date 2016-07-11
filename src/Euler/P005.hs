module Euler.P005 where

{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

calc n = head . filter canDivs $ [ps, ps*2 ..]
    where ps = product.takeWhile (<n) $ primes
          canDivs x = all ((==0).(x `rem`)) [2..n]


