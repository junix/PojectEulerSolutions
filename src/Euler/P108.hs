module P108 where
import Math.NumberTheory.Primes
import qualified  Data.Set as S
import Data.Ratio
{-
In the following equation x, y, and n are positive integers.

                                1 + 1 = 1
                                x   y   n

For n = 4 there are exactly three distinct solutions:

                                1 + 1  = 1
                                5   20   4
                                1 + 1  = 1
                                6   12   4
                                1 + 1  = 1
                                8   8    4

What is the least value of n for which the number of distinct solutions
exceeds one-thousand?

NOTE: This problem is an easier version of problem 110; it is strongly
advised that you solve this one first.
-}

nSeq n = concatMap (\x -> calcPair (n*x) x) [2,3..n+1]
    where effectiveDivisors n = takeWhile (<n) . S.toList . divisors
          selectPairs d v xs = go xs ys
              where ys = reverse xs
                    go [] _ = []
                    go _ [] = []
                    go xs'@(x:xs) ys'@(y:ys)
                        | x > y = []
                        | sxy < v = go xs ys'
                        | sxy > v = go xs' ys
                        | gcd x y == 1 =  (x%d, y%d) : go xs ys
                        | otherwise = go xs ys
                        where sxy = x + y
          calcPair d x = selectPairs d x . effectiveDivisors x . (n*) $ x

nseq n = filter canDivide [n+1..n*2]
    where canDivide x = r == 0
            where (q,r) = (x*n) `quotRem` (x-n)

euler = filter ((>1000).length.nseq) [s,s*2..]
    where s = 2*3*5*7*11*13


