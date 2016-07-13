module Euler.P043 where
import Data.List

primes = [1,2,3,5,7,11,13,17,19,23]

canDiv xs = go $ zip xs primes
  where go ys
          | length ys < 3 = True
          | otherwise =
                let hd = read . take 3 . map fst $ ys
                    pr = snd . head $ ys
                in hd `rem` pr == 0 && (go (tail ys))

perms [] = [[]]
perms xs = [x:y | x <- xs, y <- perms (delete x xs)]

euler43 :: Integer
euler43 =  sum . map read . filter canDiv . perms $ ['0'..'9']