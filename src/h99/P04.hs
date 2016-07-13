module H99.P04 where

len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs

