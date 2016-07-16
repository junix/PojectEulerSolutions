module Euler.P075 where

calcRightTriInt n = length [(x,y,z) | x <- [1..n], y <- [x..(n-x)],let z = (n-x-y), z > y, x^2+y^2==z^2]

