module Euler.P025 where

fibs = 1:1:go 1 1
    where go x y = let z = (x+y) in z : go y z

euler25 :: Integer
euler25 = fst.head.dropWhile ((<1000).length.show.snd) $ zip [1..] fibs

