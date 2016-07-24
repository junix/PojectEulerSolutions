module Main(main) where

import Data.List (nub, sort,delete)

main = do
    l <- getLine
    mapM_ putStrLn (perm l)

perm l = go.sort $ l
    where go [] = [[]]
          go xs = [ x:y |x <- xs, y <- perm (delete x xs)]

