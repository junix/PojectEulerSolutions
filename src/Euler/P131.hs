module Euler.P131 where
import Math.NumberTheory.Primes

seek n = go pseq
    where pseq = map (^3) [1..]
          go (x:xs'@(y:xs))
             | y - x > n = []
             | otherwise = ms ++ go xs'
                 where ms = filter isPrime . takeWhile (<n) . map (subtract x) $ xs'

euler = length . seek $ 1000000