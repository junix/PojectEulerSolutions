module Euler.P105 where
import Data.List (nub, minimumBy,sort,groupBy, sortBy)
import Data.Function(on)

genSet [] = [[]]
genSet (x:xs) = subset ++ map (x:) subset
    where subset = genSet xs

allSet xs = tail . genSet $ xs

verify xs = go vs
    where xss = sortBy (compare `on` length). allSet $ xs
          vs = map sort . map (map sum) . groupBy (\as bs -> length as == length bs) $ xss
          uniq xs = (length.nub) xs == length xs
          go [] = True
          go [xs] = uniq xs
          go (xs:ys:xss) = last xs < head ys && uniq xs && go (ys:xss)

main = do
    c <- readFile "./p105_sets.txt"
    print (sum . map sum . filter verify . map parseVec . lines $ c)

parseVec xs = read ('[':xs++"]") :: [Integer]
