module Euler.P033 where

import Data.Ratio

toLi :: Integer -> [Integer]
toLi x = [q, r]
    where (q,r) = x `quotRem` 10

canCancel a b = go (toLi a) (toLi b)
    where go [x1,x2] [y1,y2]
            | x2 /= y1 = False
            | y2 == 0  = False
            | otherwise = a % b == x1 % y2

euler = product [ a%b | a <- [10..99], b<- [(a+1)..99], canCancel a b]
