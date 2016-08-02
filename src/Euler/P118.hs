module Euler.P118 where
import Math.NumberTheory.Primes
import Data.List(permutations,(\\),sort,nub)
import qualified Data.Set as S

breaks xs = go [] xs
    where go _ [] = []
          go [] (x:xs)  = ([x], xs) : go [x] xs
          go xs  [y]    = [(y:xs, [])]
          go xs  (y:ys) = (acc, ys) : go acc ys
            where acc = y : xs

asInt [x] = x
asInt (x:xs) = asInt xs * 10 + x

genSet [] = [[]]
genSet xs = [ m:ts | (h,t) <- breaks xs, let m = asInt h, isPrime m, ts <- genSet t]

p118 :: [Integer] -> [[Integer]]
p118 = S.toList . S.fromList . map sort . concatMap genSet . permutations

euler = length . p118 $ [1..9]
