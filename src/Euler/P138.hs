module Euler.P138 where

cs0 = map (^2) [1..]
cs1 = map (\x -> 5*x^2-4*x + 1) [1..]
cs2 = map (\x -> 5*x^2+4*x + 1) [1..]

go (x:xs) (y:ys)
    | x == y = x : go xs ys
    | x < y = go xs (y:ys)
    | otherwise = go (x:xs) ys

