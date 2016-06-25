module Algo.Halves where

halves :: [a] -> ([a],[a])
halves xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

halves1 :: [a] -> ([a],[a])
halves1 [] = ([],[])
halves1 [x] = ([x],[])
halves1 [x,y] = ([x],[y])
halves1 (x:y:xs) = (x:xs',y:ys')
    where (xs',ys') = halves1 xs

halves2 :: [a] -> ([a],[a])
halves2 [] = ([],[])
halves2 (x:xs) = if length xs' < length ys'
                 then (x:xs', ys')
                 else (xs', x:ys')
                 where (xs', ys') = halves2 xs
