module Euler.P089 where
import Data.List(isPrefixOf)

main = do
    c <- readFile "./p089_roman.txt"
    let ls = sum . map (\(a,b) -> length a - length b) . filter (\(a,b) -> a /= b) . map (\s -> (s, conv s)). lines $ c
    print ls

data R = I | V | X | L | C | D | M deriving (Ord,Eq,Show,Read)
data N = U R  | S R R | CC [N] deriving(Eq,Show)

conv :: String -> String
conv s = int2r (val . ton $ s)

v I = 1
v V = 5
v X = 10
v L = 50
v C = 100
v D = 500
v M = 1000

val (U x) = v x
val (S a b) = v b - v a
val (CC xs) = sum . map val $ xs

toN :: [R] -> N
toN = CC . go
    where go [] = []
          go (I:V:xs) = S I V : go xs
          go (I:X:xs) = S I X : go xs
          go (X:L:xs) = S X L : go xs
          go (X:C:xs) = S X C : go xs
          go (C:D:xs) = S C D : go xs
          go (C:M:xs) = S C M : go xs
          go (x:xs)   = U x : go xs

tos :: String -> [R]
tos = map (read.(:[]))

ton ss = toN . tos $ ss

int2r n
    | n >= 1000 = replicate q0 'M' ++ int2r r0
    | q1 == 9   = "CM" ++ int2r r1
    | q1 == 4   = "CD" ++ int2r r1
    | n >= 500  = 'D' : int2r (n-500)
    | n >= 100  = replicate q1 'C' ++ int2r r1
    | q2 == 9   = "XC" ++ int2r r2
    | q2 == 4   = "XL" ++ int2r r2
    | n >= 50   = 'L':int2r (n-50)
    | n >= 10   = replicate q2 'X' ++ int2r r2
    | n == 9    = "IX"
    | n == 4    = "IV"
    | n >= 5    = 'V':int2r (n-5)
    | otherwise = replicate r2 'I'
    where (q0,r0) = n `quotRem` 1000
          (q1,r1) = n `quotRem` 100
          (q2,r2) = n `quotRem` 10
