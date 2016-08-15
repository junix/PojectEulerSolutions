module Euler.P109 where
import Data.Function (on)
import Data.List (sort,sortBy, nub)

data Dart = S Integer | D Integer | T Integer deriving (Show)
data CheckOut = CheckOut [Dart] deriving(Show)

count :: Dart -> Integer
count (S v) = v
count (D v) = v*2
count (T v) = v*3

eq (S v) (S v') = v == v'
eq (D v) (D v') = v == v'
eq (T v) (T v') = v == v'
eq _ _ = False

instance Eq Dart where
    a == b = count a == count b

instance Eq CheckOut where
    (CheckOut []) == (CheckOut []) = True
    CheckOut (x:xs) == CheckOut (y:ys) = x `eq` y && go xs' ys'
        where xs' = sort xs
              ys' = sort ys
              go [] [] = True
              go xs [] = False
              go [] ys = False
              go (x:xs) (y:ys) = x `eq` y && go xs ys

instance Ord Dart where
    compare = compare `on` count

allDarts = sortBy (flip compare) $ concat [[S v, D v, T v] | v <- [1..20]] ++ [S 25, D 25]
allDDarts = sort $ D 25 : [D v | v <- [1..20]]


lastPos n = [ d | d <- allDDarts, count d <= n]

posSeq 0 0 xs =  [[]]
posSeq c 0 xs =  [[]]
posSeq 0 n xs =  []
posSeq c n [] =  []
posSeq c n (x:xs) = [ replicate (fromInteger c') x ++ ps
                    | c' <- [0..min c q]
                    , ps <- posSeq (c-c') (n-c'*cx) xs
                    ]
        where cx = count x
              q = n `quot` cx

checkout n = nub
    [ CheckOut (l:xs)
    | l <- lps
    , xs <- posSeq  2 (n-count l) allDarts
    ]
    where lps = lastPos n

p109 = sum . map (length.checkout) $ [1..99]
