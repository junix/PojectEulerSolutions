module Euler.P037 where
import Math.NumberTheory.Primes
import Data.List
import Data.Bits ((.&.))


isTruncPrime :: String -> Bool
isTruncPrime = all isPrime . lrseq

lrseq :: String -> [Integer]
lrseq xs = map read $ (mid . inits) xs ++ (mid . tails) xs
    where mid []  = []
          mid [x] = []
          mid xs  = tail . reverse . tail $ xs

euler37 = sum . map read . take 11 . filter isTruncPrime . map show . drop 4 $ primes




