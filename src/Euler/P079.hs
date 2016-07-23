module Euler.P079 where
import Data.List (sort,nub, partition)

stats [] = []
stats (x:xs) = zip (repeat x) xs ++ stats xs

topoOrder ms = go freqDict []
    where orderSet = sort . nub . concatMap stats $ ms
          freqDict = [(k, map snd . filter ((==k).fst) $ orderSet) | k <- (sort.nub.concat $ ms)]
          go [] acc = acc
          go xs acc = go remain (delcs ++ acc)
            where (hd,tl) = partition (null.snd) xs
                  delcs   = sort . map fst $ hd
                  remain  = map (del delcs) tl

del cs (c,xs) = (c,filter (`notElem` cs) xs)

main = do
    ms <- lines <$> readFile "./p079_keylog.txt"
    print $ topoOrder ms
