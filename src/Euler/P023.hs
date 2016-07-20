module Euler.P023 where
import Math.NumberTheory.Primes
import Data.List (sort,nub, (\\))
import Data.Set (toList)

select x c = map (x^) [0..c]

ds [] = [1]
ds ((p,c):xs) = sort [ s*o | s <- select p c, o <- ds xs]

divisorSeq :: Integer -> [Integer]
divisorSeq = init . ds . factorise

isAbundant n = (>n). sum .init .toList. divisors$ n

abundantSeq =filter isAbundant $ [1..38123]

allAbSum = allAbSum' abundantSeq abundantSeq

allAbSum' xs [] = []
allAbSum' [] ys = []
allAbSum' xs'@(x:xs) ys'@(y:ys)
    | x > y     = []
    | s < 28324 = s : allAbSum' xs' ys
    | otherwise = allAbSum' xs ys'
    where s = x + y


