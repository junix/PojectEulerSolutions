module Euler.P003 where

maxFactor = go primes
    where go (x:xs) v
            | x > v     = v
            | r == 0    = max x (go primes d)
            | otherwise = go xs v
            where (d,r) = v `quotRem` x

primes = filterP [2..]
  where  filterP (x:xs) = x : (filterP . filter (not.canDiv)) xs
            where canDiv v = v `rem` x == 0

factorise n = go n ps
  where r0 = truncate . sqrt . fromInteger $ n
        ps = takeWhile (<=r0) primes
        go 1 _ = []
        go n [] = [n]
        go n xs'@(x:xs)
          | r == 0 = x : go q xs'
          | otherwise = go n xs
          where (q,r) = n `quotRem` x

solve = maximum . factorise $ 600851475143
