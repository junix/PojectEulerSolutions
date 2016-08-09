module Euler.P105 where
import Data.List (nub, minimumBy,sort,groupBy, sortBy)
import Data.Function(on)

genSet []     = [[]]
genSet (x:xs) = subset ++ map (x:) subset
    where subset = genSet xs

noneEmptySubSet :: [Integer] -> [[Integer]]
noneEmptySubSet = tail . genSet

verify xs = go vs
    where vs = map     (sort . map sum)      .
               groupBy ((==) `on` length)    .
               sortBy  (compare `on` length) .
               noneEmptySubSet               $
               xs
          isUniq xs = (length . nub) xs == length xs
          go []          = True
          go [xs]        = isUniq xs
          go (xs:ys:xss) = last xs < head ys && isUniq xs && go (ys:xss)

main = readFile "./p105_sets.txt" >>= print . euler

euler :: String -> Integer
euler = sum           .
        map sum       .
        filter verify .
        map parseVec  .
        lines

parseVec xs = read ('[':xs++"]") :: [Integer]
