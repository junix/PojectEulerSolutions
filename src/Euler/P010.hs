module Euler.P010 where
import Math.NumberTheory.Primes

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
-}

intSqrt::Integer -> Integer
intSqrt = floor . sqrt . fromInteger

--isPrime::Integer->Bool
--isPrime n = and $ map (\x -> (n `rem` x) /= 0) [2..intSqrt n]

--calc1 =  sum $ filter isPrime [2..2000000]

calc n = sum . takeWhile (<n) $ primes
