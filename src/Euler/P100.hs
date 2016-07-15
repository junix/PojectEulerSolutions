module Euler.P100 where

arrange :: Integer -> [Integer]
arrange n = go [root..n]
    where prod = n * (n-1) `quot` 2
          root = truncate.sqrt.fromInteger $ prod

          go [] = []
          go (x:xs)
            | s == prod = [x]
            | s > prod  = []
            | otherwise = go xs
            where s = (x-1)*x


euler100 = head. concatMap arrange $ [10^12..]


f x y
  | n >= 10^12 = (x+2*y+1) `div` 2
  | otherwise = f (3*x+8*y) (3*y+x)
  where n = (x+1) `div` 2 + 2 * y

main = print $ f 3 1