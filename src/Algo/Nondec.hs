module Algo.Nondec where

nondec :: (Ord a) => [a] -> Bool
nondec []       = True
nondec [x]      = True
nondec (x:y:xs) = (x <= y) && nondec (y:xs)

nondec1 :: (Ord a) => [a] -> Bool
nondec1 []  = True
nondec1 xs  = and $ zipWith (<=) xs (tail xs)
