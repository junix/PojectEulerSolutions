module Euler.P187 where
import Math.NumberTheory.Primes

euler = go primes
    where limit = 10^8
          go xs'@(x:xs)
              | x*x > limit = 0
              | otherwise = lenx + go xs
              where lenx = length . takeWhile (<limit) . map (x*) $ xs'

