module Euler.P029 where
import Data.List (nub,sort)

euler = length . sort . nub $ [x^y | x <- [2..100], y<- [2..100]]

