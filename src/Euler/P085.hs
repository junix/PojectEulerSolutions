module Euler.P085 where

squareCnt x y = abs (x*(x+1)*y*(y+1) `quot` 4 - 2000000)

euler = uncurry (*) . snd . head $ go allPerm []
    where allPerm = [(squareCnt x y, (x, y))| x <- [1..2000], y <- [1..2000]]
          go [] acc        = acc
          go (x:xs) []     = go xs [x]
          go (x'@(x,_):xs) ys'@(y'@(y,_):ys)
             | x == y =  go xs (x':ys')
             | x >  y =  go xs ys'
             | otherwise = go xs [x']






