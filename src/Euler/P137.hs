module Euler.P137 where
import Data.Ratio

fibs = 1 : 1 : go 1 1
    where go a b = a + b : go b (a+b)

sseq = go . tail $ fibs
    where go (x:y:xs) = x*y : go xs

euler = sseq !! 14
