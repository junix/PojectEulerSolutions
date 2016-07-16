module Euler.P072 where
import Data.Ratio
import Math.NumberTheory.Primes

fractions n = product .map (\(a,b) -> (a^(b-1)*(a-1))).factorise $ n

euler n = sum . map fractions $ [2..n]

