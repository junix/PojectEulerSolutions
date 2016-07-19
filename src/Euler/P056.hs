module Euler.P056 where

import Data.Char (ord)

euler = maximum [digitsSum (a^b)| a <- [1..100], b <- [1..100]]

digitsSum :: Integer -> Int
digitsSum n = sum . map ((subtract (ord '0')).ord) . show $ n

