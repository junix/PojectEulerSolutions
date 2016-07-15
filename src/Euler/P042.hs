module Euler.P042 where

import Data.List(sort)
import Data.Char(ord)

main = do
    content <- readFile "p042_words.txt"
    print $ euler (read content :: [String])

triSeq = map (\x->x*(x+1) `quot` 2) [1..]

toInt :: String -> Int
toInt = sum . map (\x -> ord x - ord 'A' + 1)

euler :: [String] -> Int
euler xss = go triSeq (sort.map toInt $ xss)
    where go _ []      = 0
          go (x:xs) ys = (length . dropWhile (<x) $ hs) + go xs ts
            where (hs,ts) = break (>x) ys


