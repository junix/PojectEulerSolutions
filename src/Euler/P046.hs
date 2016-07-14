module Euler.P046 where
import Math.NumberTheory.Primes

canComposite :: Integer -> Bool
canComposite n = go [1..(truncate.sqrt.fromInteger) n]
    where go []     = False
          go (x:xs)
           | remain <= 0 = False
           | otherwise = isPrime remain || go xs
           where remain = n - 2*x^2

euler46 = head .dropWhile canComposite . filter (not.isPrime) $ [3,5..]

