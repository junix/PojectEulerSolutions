module Euler.P101 where
import Data.List (delete)
import Data.Ratio

f x = sum . zipWith (*) coef . map (x^) $ [0..10]
    where coef = cycle $ [1,-1]

nseq :: [Integer]
nseq = map f [1..11]

fn :: Integer -> Integer -> Integer
fn n m =  numerator . sum $ [ p x y m | (x,y) <- points ]
    where points = zip [1..] . take (fromInteger n) $ nseq
          p x y = \v -> y % (product [ x - xk |(xk,yk) <- rs]) *  (product [ v - xk | (xk,yk) <- rs] % 1)
               where rs = delete (x,y) points

euler :: Integer
euler = sum [ fn n (n+1) | n <- [1..10]]