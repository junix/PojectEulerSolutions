module Main where
import Math.NumberTheory.Primes
import Control.Parallel.Strategies
import Data.List (isSuffixOf)

pseq = reverse $ zip s (dropWhile (<=5) primes)
    where s =  dropWhile (<5). takeWhile (<=10^6) $ primes

con (a,b) = go 1 rb
    where r = rem10 a
          rb = r `rem` b
          sr = b-a
          go c y
            | y == sr = c*r  + a
            | otherwise = go (c+1) ((y+rb) `rem` b)

rem10 :: Integer -> Integer
rem10 = (10^) . ceiling . logBase 10 . fromInteger

cont (a,b) = go 1 rb
    where r = rem10 a
          rb = b `rem` r
          go c y = if y `rem` r == a
                   then c*b
                   else go (c+1) y'
              where y' = (y + rb) `rem` r

euler = map con . reverse $ pseq

main = print euler