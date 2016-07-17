module Euler.P123 where
import Math.NumberTheory.Primes

remainder (n,p) = ((p-1)^n  + (p+1)^n) `rem` (p^2)

p = dropWhile ((<10^10).snd) . map (\(n,p)-> (n,remainder (n, p))) . filter (odd.fst) . zip [1..] $ primes

