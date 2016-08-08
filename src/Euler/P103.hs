module Euler.P103 where
import Data.List (nub, minimumBy,sort,groupBy, sortBy)
import Data.Function(on)

set 1 = [1]
set 5 = [6, 9, 11, 12, 13]
set 6 = [11,18,19,20,22,25]
set n = bound
    where prev = set (n-1)
          l = length prev
          index = if even l then (l `quot` 2) else (l-1) `quot` 2
          e = prev !! index
          bound = e : map (e+) prev

genSet [] = [[]]
genSet (x:xs) = subset ++ map (x:) subset
    where subset = genSet xs

allSet xs = tail . genSet $ xs

verify' xs = go vs
    where xss = sortBy (compare `on` length). allSet $ xs
          vs = map sort . map (map sum) . groupBy (\as bs -> length as == length bs) $ xss
          uniq xs = (length.nub) xs == length xs
          go [] = True
          go [xs] = uniq xs
          go (xs:ys:xss) = last xs < head ys && uniq xs && go (ys:xss)

search n = filter verify' $ go n minc sumc
    where cs = sort.set $ n
          minc = head cs
          sumc = sum cs

go c from remain
    | c == 0 = [[]]
    | from > remain = []
    | otherwise = [ v:vs | v <- [from..remain `quot` c]
                         , let r = remain -v
                         , let c' = c - 1
                         , vs <- go c' (v+1) r]