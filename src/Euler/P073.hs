module Euler.P073 where
import Data.Ratio

count :: Int -> Int
count d = go [d`quot`3..d`quot`2]
    where go [] = 0
          go (n:ns) = if outOfRange || canReduce then go ns else 1 + go ns
            where outOfRange = 2*n >= d || d >= 3*n
                  canReduce = numerator (n%d) == n

euler = sum . map count $ [2..12000]

