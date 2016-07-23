module Euler.P060 where
import Data.List (sort , nub, isPrefixOf, isSuffixOf, intersect)

import Math.NumberTheory.Primes

isConsPr :: Integer -> Bool
isConsPr = not . null . splitCons

splitCons :: Integer -> [(Integer, Integer)]
splitCons n = go ([c]) cs
    where (c:cs) = show n
          go _ [] = []
          go xs ys'@(y:ys) = if isPrime rn && isPrime yn && isStrPr zs
                             then (rn, yn) : next
                             else next
            where rs = reverse xs
                  rn = read rs
                  yn = read ys'
                  zs = ys' ++ rs
                  next = go (y:xs) ys

isCons a b = isPrime a && isPrime b && isPrime c && isPrime d
    where c = read (show a ++ show b)
          d = read (show b ++ show a)

canCarve pred carveOp n consn = if pred sc && isCons n m then [m] else []
    where sc = show consn
          m = read . carveOp $ sc :: Integer

isStrPr :: String -> Bool
isStrPr = isPrime . read

consSeq = filter isConsPr $ primes

tryPair xs (a,b) = sort . nub $ (try a xs `intersect` try b xs)

try a xs = concatMap p1 xs `intersect` concatMap p2 xs
    where sa = show a
          lsa = length sa
          pred s = sa /= s && (sa `isPrefixOf` s)
          carve = drop lsa
          p1 = canCarve pred carve a
          pred2 s = sa /= s && (sa `isSuffixOf` s)
          carve2 :: String -> String
          carve2 xs = reverse . drop lsa . reverse $ xs
          p2 = canCarve pred2 carve2 a

euler = go [] consSeq
    where go acc (x:xs) = if null u then go (x:acc) xs else u : go (x:acc) xs
               where ps = splitCons x
                     m = filter ((>2).length.fst) . map (\pr -> (tryPair acc pr, pr))  $ ps
                     u = concatMap f m

f (cs,ab) = if null t then [] else [(head t, ab)]
    where t = selectP cs

selectT [] = []
selectT [_] = []
selectT [_,_] = []
selectT [a,b,c] = if isCons a b && isCons b c && isCons a c then [(a,b,c)] else []
selectT (x:xs) = [(x,a,b) | (a,b) <- selectP xs, isCons x a, isCons x b] ++ selectT xs

selectP [] = []
selectP [_] = []
selectP [a,b] = if isCons a b then [(a,b)] else []
selectP (x:xs) = (map (\e -> (x,e)) . filter (isCons x) $ xs) ++ selectP xs

