module P243 where
import Math.NumberTheory.Primes
import Data.List (delete,union)
import Data.Ratio
import qualified Data.Set as Set
{-
A positive fraction whose numerator is less than its denominator is called
a proper fraction.
For any denominator, d, there will be d−1 proper fractions; for example,
with d = 12:
1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 ,
8/12 , 9/12 , 10/12 , 11/12 .

We shall call a fraction that cannot be cancelled down a resilient
fraction.
Furthermore we shall define the resilience of a denominator, R(d), to be
the ratio of its proper fractions that are resilient; for example, R(12) =
4/11.
In fact, d = 12 is the smallest denominator having a resilience R(d) <
4/10.

Find the smallest denominator d, having a resilience R(d) < 15499/94744.
-}

tuple xs 1 = map (:[]) xs
tuple xs n = [ x:ys | x <- xs, ys <- tuple (dropWhile (<=x) xs) (n-1) ]

r n = (n - 1 - (go . zip (cycle [1,-1]) $ [1..length fs])) % n
    where fs = map fst . factorise $ n
          go [] = 0
          go ((sign, tupleLen):xs) = sign*c + go xs
              where c = sum . map (((n-1) `quot`) . product) . tuple fs $ tupleLen

eulerPhi n = 1 % (n-1) * phi
    where ps = map fst . factorise $ n
          phi = (n % 1 *) . product $ [ 1 - 1%p | p <- ps]

ns = go 1 primes
    where go n (x:xs) = n' : go n' xs
              where n' = n*x


euler = filter (<15499%94744) . map eulerPhi $ ns

