module Euler.P002 where

calc = sum . filter even . takeWhile (<=4000000) $ fibs
    where fibs = 1:2:fibs' 1 2
          fibs' r l = n : fibs' l n
            where n = l + r

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

pe01 = sum . filter even . takeWhile (<=4000000) $ fibs
