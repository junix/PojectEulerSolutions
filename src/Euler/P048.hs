module Euler.P048 where

euler48 :: Integer
euler48 = (`rem` (10^10)). sum . map (\x->(x^x) `rem` 10^10) $ [1..1000]

