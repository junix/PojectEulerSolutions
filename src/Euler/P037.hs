module Euler.P037 where
import Math.NumberTheory.Primes
import Data.List
import Data.Bits ((.&.))

isTruncPrime :: String -> Bool
isTruncPrime xs = all isPrime . map read $ (tail.inits) xs ++ (init . tails) xs

euler37 = sum . map read . take 11 . filter isTruncPrime . map show . drop 4 $ primes




