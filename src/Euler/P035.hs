module Euler.P035 where
import Math.NumberTheory.Primes

cyclen :: Integer -> [Integer]
cyclen x = tail . take l $ go (cycle s)
  where s = show x
        l = length s
        go (x:xs) = read (x:(take (l-1) xs)) : go xs

isCycPri n = all isPrime $ cyclen n

euler35 = length . filter isCycPri . takeWhile (<10^6) $ primes
