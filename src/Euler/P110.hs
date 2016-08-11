module Euler.P110 where
import Math.NumberTheory.Primes
import qualified  Data.Set as S
import Data.Ratio
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

euler n = head . map (length.nseq) $ [s,s*2..]
    where s = product . take n $ primes


