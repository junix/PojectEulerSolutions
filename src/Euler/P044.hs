module Euler.P044 where

penSeq = map (\x -> x*(3*x-1) `quot` 2) [1..]

isPen n = i*(3*i-1) `quot` 2 == n
    where r = truncate.sqrt $(1+24* fromInteger n)
          i = (1+ r) `quot` 6

calcPair xs ys n = go xs ys
    where go [] _ = []
          go _ [] = []
          go (x:xs) (y:ys)
            | x >= n || y >= n || x >= y = []
            | s == n && isPen d = [(x,y)] --if isPen d then [d] else []
            | s == n = go xs ys
            | s < n = go xs (y:ys)
            | otherwise = go (x:xs) ys
            where s = x + y
                  d = y - x

calc = (0-).uncurry (-). head $ go [] penSeq
    where go acc (x:xs) = calcPair penSeq acc x ++ go (x:acc) xs


