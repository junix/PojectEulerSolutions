module Euler.P070 where

import Math.NumberTheory.Primes
import Data.List (sort)
import Data.Ratio

tot n = foldl (\acc p -> acc*(p-1) `quot` p) n factors
   where factors = map fst . factorise $ n

isPerm x y = reg x == reg y
    where reg xs = sort . show $ xs

tseq = [(n % y,n) | n <- [1..10^7], let y = tot n, isPerm y n]


