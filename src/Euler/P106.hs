{-# LANGUAGE ParallelListComp #-}
module P103 where
import Data.List (nub,sort,sortBy,groupBy,intersect)
import Data.Function (on)
{-
Let S(A) represent the sum of elements in set A of size n. We shall call
it a special sum set if for any two non-empty disjoint subsets, B and C,
the following properties are true:

 1. S(B) S(C); that is, sums of subsets cannot be equal.
 2. If B contains more elements than C then S(B) > S(C).

For this problem we shall assume that a given set contains n strictly
increasing elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained
from a set for which n = 4, only 1 of these pairs need to be tested for
equality (first rule). Similarly, when n = 7, only 70 out of the 966
subset pairs need to be tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need
to be tested for equality?

NOTE: This problem is related to problems 103 and 105.
-}
-- generate all subset
powerset []     = [[]]
powerset (x:xs) = map (x:) subSet ++ subSet
    where subSet = powerset xs

-- group same length subset together
groupedSet xs = groupBy ((==) `on` length) . sortBy  (compare `on` length) . sort . powerset $ xs

-- calc how many cmp in this set
calcCheckCnt []   = 0
calcCheckCnt [xs] = 0
calcCheckCnt (xs:xss) = (length . filter (needCmp xs)) xss + calcCheckCnt xss

p106 xs = sum . map calcCheckCnt . groupedSet $ xs

-- whether need cmp
needCmp xs ys = length xs > 1 &&
                (null $ xs `intersect` ys) &&
                (((/=1).length.nub) [ x `compare` y | x <- xs | y <- ys])

main = print.p106 $ ['a'..'l']