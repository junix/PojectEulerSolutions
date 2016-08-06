module Euler.P091 where
import Data.List (sort)

pseq x y = [ pos | x0 <- [0..x], y0 <- [0..y], let pos = (x0,y0), pos /= (0,0)]

tseq x y = [(a, b) | a <- pseq x y, b <- pseq x y, a < b]

type Point = (Integer,Integer)

len :: Point -> Point -> Integer
len (x0,y0) (x1,y1) = sum . map ((^2) . fromInteger) $ [x0-x1, y0-y1]

isRightAngle (a,b) = x + y == z
    where o = (0,0)
          [x,y,z] = sort [len a o, len a b, len b o]

euler n =  filter isRightAngle $ tseq n n

