module Euler.P079 where
import Data.List (sort,nub)
import qualified Data.Map as M

stats [] = []
stats (x:xs) = zip (repeat x) xs ++ stats xs

topoOrder ms = go freqDict []
    where orderSet = sort . nub . concatMap stats $ ms
          freqDict = [(k, map snd . filter ((==k).fst) $ orderSet) | k <- (sort.nub.concat $ ms)]
          go [] acc = acc
          go xs acc = go t (delcs ++ acc)
            where delcs = sort . map fst . filter (null.snd) $ xs
                  t = map (del delcs) . filter (not.null.snd) $ xs

del cs (c,xs) = (c,filter (not.(`elem` cs)) xs)

main = do
    ms <- lines <$> readFile "./p079_keylog.txt"
    print $ topoOrder ms
