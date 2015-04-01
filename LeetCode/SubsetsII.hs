{-
    Given a set of distinct integers, S, return all possible subsets.
    [note] Elements in a subset must be in non-descending order.
    The solution set must not contain duplicate subsets.
    For example, If S = [1,2,3], a solution is:
    [ [3], [1], [2], [1,2,3], [1,3], [2,3], [1,2], [] ]
-}

import Data.List

splits :: (Eq a) => [a] -> [[[a]]]
splits [] = []
splits l@(x:xs) = let (same,remain) = span (==x) l
                      enums = scanr (:) [] same
                  in  enums : splits remain

genAllSet xs = sort.map (sort.concat).sequence.splits.sort $ xs
