module Euler.P124 where
import Math.NumberTheory.Primes
import Data.List (sort)

rad n =product . map fst . factorise $ n

stream = map (\x->(rad x, x)) $ [1..]

euler = last . take 10000 . sort . take (100000) $ stream

