module Euler.P203 where

import Math.NumberTheory.Primes
import Data.List (nub,sort)

binSeq = go [1]
    where go xs = xs : go xs'
             where xs' = 1 : zipWith (+) xs (tail xs) ++ [1]

euler = sum                           .
        filter (not.isSquareFactored) .
        sort                          .
        nub                           .
        concat                        .
        take 51                       $
        binSeq

isSquareFactored n = any (>1) es
    where es = map snd . factorise $ n