module Euler.P026 where

calcCycle n = map id (go 1 n [])
  where go remain n acc
            | r == 0 = []
            | q == 0 || null t = go (r*10) n ((q,r):acc)
            | otherwise = (q,r): reverse h
            where (q,r) = remain `quotRem` n
                  (h,t) = break (==(q,r)) acc

euler =  snd . maximum . map (\x -> ((length.calcCycle) x, x)) $ [2..1000]