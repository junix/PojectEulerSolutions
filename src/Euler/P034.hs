module Euler.P034 where

euler34 = filter dfactors' $ [10..3265920]

dfactors :: Integer -> Bool
dfactors n = (==n).sum . map (factor.read.(:[])) . show $ n

dfactors' n = go n n
    where go a l
            | a < 0 =  False
            | a == 0 = l == 0
            | l == 0 =  False
            | otherwise = go (a-factor r) d
            where (d,r) = l `quotRem` 10

factor n = product [1..n]


