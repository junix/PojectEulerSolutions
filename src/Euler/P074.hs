--module Main(main) where

import qualified Data.Set as S
import Control.Parallel.Strategies

prod 0 = 1
prod 1 = 1
prod 2 = 2
prod 3 = 6
prod 4 = 24
prod 5 = 120
prod 6 = 720
prod 7 = 5040
prod 8 = 40320
prod 9 = 362880

digits n
    | n < 10 = [n]
    | otherwise = let (q,r) = n `quotRem` 10 in r : digits q

digitsProd n = sum [prod d | d <- digits n]

chain x = go S.empty x
    where go set n
            | contained = S.size set
            | otherwise = go (S.insert n set) (digitsProd n)
            where contained = S.member n set


calc (s,e) = go s 0
    where go x acc
            | x > e = acc
            | otherwise = if chain x == 60 then go (x+1) (acc+1) else go (x+1) acc

divideRange n = go 0
    where end = 10^6 - 1
          len = end `quot` n
          go s
            |s > end = []
            |otherwise = (s, e) : go (e+1)
            where e = min (s + len) end

euler n = sum (map calc rs `using` parList rseq)
    where rs = (divideRange n)

main = do
    print $ euler 1000