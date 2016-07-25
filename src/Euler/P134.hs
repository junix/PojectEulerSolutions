module Main where
import Math.NumberTheory.Primes
import Control.Parallel.Strategies
import Data.List (isSuffixOf)

pseq = reverse $ zip s (dropWhile (<=5) primes)
    where s =  dropWhile (<5). takeWhile (<=10^6) $ primes

getCon :: (Integer, Integer) -> Integer
getCon (a,b) = head [ v | n <- [3,5..], let v = n*b, a `isSuffix` v]

getCon'' (a,b) = head [ v | n <- [1..], let v = n * base + a,  v `rem` b == 0]
    where base = 10^(length (show a))

isSuffix 0 b = True
isSuffix a b
    | ra == rb = isSuffix qa qb
    | otherwise = False
    where (qa,ra) = a `quotRem` 10
          (qb,rb) = b `quotRem` 10

divideSet n [] = []
divideSet n xs = take n xs : divideSet n (drop n xs)

calc xs = sum  (map getCon'' xs)

--euler = (map (\xs -> (head xs,calc xs)) (divideSet 100 pseq) `using` parList rseq)
euler = sum (map getCon'' pseq `using` parList rseq)

main = print euler