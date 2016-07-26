module Euler.P078 where


import Data.Array ( (!),elems,listArray )

a = listArray (0,10^5) $ [f i | i <- [0..]]; f 0 = 1
f n = let xs = 1 : 1 : -1 : -1 : xs in sum $ zipWith (*) xs [a!(n-k) | k <- takeWhile
  (<=n) [div (3*n*n-n) 2 | n <- concatMap (\(a,b) -> [a,b]) $ zip [1..] [-1,-2..]]]
main = let n = 10^6 in print $ length $ takeWhile ((/=0) . flip mod n) $ elems a

