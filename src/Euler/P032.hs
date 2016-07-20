module Euler.P032 where
import Data.List (permutations, nub)

calcPanProd xs = prods h p
    where (h,t) = splitAt 5 xs
          p = asInt t

asInt [] = 0
asInt (x:xs) = (asInt xs) * 10 + x

prods :: [Integer] -> Integer -> [Integer]
prods xs p = if null seqs then [] else [p]
    where l = length xs
          seqs = filter (== p) .
                 map (\(x,y) -> asInt x * asInt y) .
                 map (flip splitAt xs) $
                 [1..l-1]
pseq = concatMap calcPanProd . permutations $ [1..9]

euler = sum.nub $ pseq

