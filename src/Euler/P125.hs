module Euler.P125 where
import Data.List (nub)

stream = map (^2) [1..]

isPlind :: Integer -> Bool
isPlind = adjust . show
    where adjust n = let s = show n in s == reverse s

plindN n (x:xs) = let s = sum $ x:(take (n-1) xs)
              in if isPlind s
                 then s: plindN n xs
                 else if s < 10^8
                      then plindN n xs
                      else []

euler = sum.nub.concat $ [plindN nseq stream | nseq <- [2..1000]]