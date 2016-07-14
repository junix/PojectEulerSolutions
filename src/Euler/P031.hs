module Euler.P031 where

costs = [200,100,50,20,10,5,2,1]

paths 0 [] = [[]]
paths _ [] = []
paths total (x:xs) = [(x,c):y | c <- [0..(total `div` x)], y <- paths (total-c*x) xs ]
euler34 = length . paths 200 $ costs


npaths :: [Integer] -> Integer -> Integer
npaths _ 0   = 1
npaths [c] _ = 1
npaths (x:xs) total = sum $ map (npaths xs . (total-)) [0,x..total]

euler34' = npaths costs 200

x = print $ count [200,100,50,20,10,5,2,1] 200

count :: [Integer] -> Integer -> Integer
count _ 0      = 1
count [c] _    = 1
count (c:cs) s = sum $ map (count cs . (s-)) [0,c..s]