module Euler.P038 where
import Data.List (nub)

calcPandNum n = go n 1 (show n)
    where go n c acc
            | lenAcc > 9  = []
            | lenAcc == 9 && isPand = [acc]
            | lenAcc == 9 = []
            | otherwise   = go n (c+1) (acc++show (n*(c+1)))
            where lenAcc = length acc
                  isPand = (length.nub.filter (/='0')) acc == 9 && n > 1

euler =  maximum . concatMap calcPandNum $ [192..100000]
