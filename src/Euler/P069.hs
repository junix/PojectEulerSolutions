module Euler.P069 where

import Math.NumberTheory.Primes
import Data.Ratio

tot n = foldl (\acc p -> acc*(p-1) `quot` p) n factors
   where factors = map fst . factorise $ n

euler = maximum . map (\x -> (x % (tot x),x)) $ [1..10^6]


