module Euler.P102 where
import Data.List.Split (wordsBy)

type Point = (Integer,Integer)

sign :: Point -> Point -> Point -> Integer
sign (x1,y1) (x2,y2) (x3,y3) = (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)

inTriangle a b c p = b1 == b2 && b2 == b3
    where b1 = sign p a b < 0
          b2 = sign p b c < 0
          b3 = sign p c a < 0

containOrig :: Point -> Point -> Point -> Bool
containOrig a b c = inTriangle a b c (0,0)

readTriangle :: IO [[(Integer,Integer)]]
readTriangle = do
    c <- readFile "./p102_triangles.txt"
    return (map (toPoints . map read . (wordsBy (==','))) . lines $ c)

toPoints [] = []
toPoints (x:y:xs) = (x,y) : toPoints xs

main = do
    tris <- readTriangle
    let res = [(a,b,c) |[a,b,c] <- tris, containOrig a b c]
    print $ length res