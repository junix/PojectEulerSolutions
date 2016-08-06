module Main(main) where
import Data.List (nub)
import qualified Data.Set as S
import Math.NumberTheory.Primes
import Control.Parallel.Strategies

isAreaIntegral a
    | even a = []
    | otherwise = if isSquareNo (h2*b2)
                      then [(a,a+1)]
                      else if isSquareNo (h2'*b2')
                              then [(a-1,a)]
                              else []
    where b2 = ((a+1) `quot` 2)^2
          b2' = ((a-1) `quot` 2)^2
          h2 = a^2 - b2
          h2' = a^2 - b2'

isSquareNo :: Integer -> Bool
isSquareNo 0 = True
isSquareNo x = all even . map snd . factorise $ x

fs x = go f1 f3
    where f1 = factorise (x-1)
          f3 = factorise (3*x+1)
          go [] ys = all (even.snd) ys
          go xs [] = all (even.snd) xs
          go xs'@((x,ex):xs) ys'@((y,ey):ys)
            | x == y = even (ex + ey) && go xs ys
            | x < y  = even ex && go xs ys'
            | otherwise  = even ey && go xs' ys

fs' x =  r1 ==0 && r2 == 0 && f1 && f2
    where (q1,r1) = (x-1) `quotRem` 4
          (q2,r2) = (3*x+1) `quotRem` 4
          f1 = all even . map snd . factorise $ q1
          f2 = all even . map snd . factorise $ q2

solve (s,e) = filter fs' [s',s'+2..e]
    where s' = if even s then s + 1 else s

maxLen = (10^9-1) `quot` 3

divs step = go step 1
    where go step c
            | c > maxLen = []
            | c + step >= maxLen = [(c,maxLen)]
            | otherwise = (c,c+step) : go step (c+step+1)

euler = concat $
        (map solve1 rs `using` parList rseq)
    where rs = divs 100000

solve1 (s,e) = concatMap isAreaIntegral [s..e]

main = mapM_ print euler

