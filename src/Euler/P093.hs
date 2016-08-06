module P093 where
import Data.List (delete,permutations,nub,sort)
import Data.Maybe (maybeToList)
import qualified Data.IntSet as S
import Data.Ratio
{-
By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
making use of the four arithmetic operations (+, , *, /) and
brackets/parentheses, it is possible to form different positive integer
targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3) 1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
target numbers of which 36 is the maximum, and each of the numbers 1 to 28
can be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which the longest
set of consecutive positive integers, 1 to n, can be obtained, giving your
answer as a string: abcd.
-}

select n xs = go n xs
    where go  0 _ = [[]]
          go  c ys = [ x:zs | x <- ys, zs <- go (c-1) (dropWhile (<=x) ys)]

dseq = select 4 [1..9]

opseq :: [Ratio Int -> Ratio Int -> [Ratio Int]]
opseq = [ad,sb,mt,qt]

gen :: [Int] -> (Int,[Int])
gen ds = (\x->(x,ds)) . maxSeqNo . S.toList . S.fromList . calc . map (%1) $ ds

ad x y = [x+y]
sb x y = [x-y]
mt x y = [x*y]
qt x y
    | y == 0 = []
    | otherwise = [x/y]

calc :: [Ratio Int] -> [Int]
calc [] = []
calc [x] = if denominator x == 1 &&  numerator x > 0 then [numerator x] else []
calc xs = concatMap calc . concatMap op . sel 2 $ xs

maxSeqNo xs = go xs 1 0
    where go [] _ no = no
          go (y:ys) v no
             | y == v = go ys (v+1) v
             | otherwise = no

op :: ([Ratio Int], [Ratio Int]) -> [[Ratio Int]]
op ([x,y],xs) = concatMap exe $ opseq
    where exe f = do
              v <- f x y
              [v:xs]

sel n xs = concatMap (\(as,bs) -> [(as,bs), (reverse as,bs)]) .
           map (\(as,bs) ->(sort as, sort bs)) $
           go n xs []
    where go 0 xs acc = [([], xs++acc)]
          go n [] acc = []
          go n (x:xs) acc = map join seled ++ skiped
            where join (ss, rs) = (x:ss, rs)
                  seled  = go (n-1) xs acc
                  skiped = go n xs (x:acc)

euler = maximum . map gen $ dseq