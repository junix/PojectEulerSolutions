module Euler.P023 where
import Math.NumberTheory.Primes
import Data.List (sort,nub, (\\))
import qualified Data.Set as S

select x c = map (x^) [0..c]

ds [] = [1]
ds ((p,c):xs) = sort [ s*o | s <- select p c, o <- ds xs]

divisorSeq :: Integer -> [Integer]
divisorSeq = init . ds . factorise

isAbundant n = (>n). sum .init .S.toList. divisors$ n

abundantSeq n =filter isAbundant $ [1..n]

allAbSum n = allAbSum' n S.empty (abundantSeq n)

allAbSum' n set [] = set
allAbSum' n set (x:xs)
    | 2*x > n = set
    | otherwise = allAbSum' n nset xs
    where values = takeWhile (<=n) .map (x+) $ (x:xs)
          nset = foldl (\s v -> S.insert v s) set values


euler n = [1..n] \\ S.toList (allAbSum n )

