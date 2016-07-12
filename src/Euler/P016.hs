module Euler.P016 where

euler16 :: Integer
euler16 = sum . map (read . (:[])) . show . (2^) $ 1000

