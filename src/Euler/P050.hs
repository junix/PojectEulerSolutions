module Euler.P050 where

import Math.NumberTheory.Primes
import Data.List (splitAt)

try n = go s ini xs
    where (ini, xs) = splitAt n primes
          s = sum ini
          go s (c:cs) (x:xs)
            | s >= 10^6 = []
            | isPrime s = [s]
            | otherwise = go (s-c+x) (cs++[x]) xs

euler50 = head.concatMap try $ [1000,999..21]
