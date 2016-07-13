module Euler.P047 where

import Math.NumberTheory.Primes

calc = go $ map (\x -> ((length . factorise) x, x)) $ [100..]
    where go ((4,a):(4,_):(4,_):(4,_):_) = a
          go (x:xs) = go xs


