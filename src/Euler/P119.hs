module Euler.P119 where
import Data.List (nub,sort)

digitsSumOf n
    | n < 10 = n
    | otherwise = go n
        where go 0 = 0
              go x = r + go q
                 where (q,r) = x `quotRem` 10

powerEq n = filter ((==n).digitsSumOf) . map (n^) $ [2..99]

euler = (!!29) . sort . concatMap powerEq $ [2..99]
