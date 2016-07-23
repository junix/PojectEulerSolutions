module Euler.P062 where
import Data.List (sort)
import qualified Data.Map.Strict as M

cubeSeq = map (^3) [1..]
vCubeSeq = zip [1..] cubeSeq

tag :: Integer -> (Int, Integer)
tag  = (\xs -> (length xs, read xs)). sort . show

tagedSeq = map (\x -> (tag x, x)) cubeSeq

seek n = takeWhile ((<n).fst) $ go 0 M.empty (map fst tagedSeq)
    where go n m xs'@(k@(nd,v):xs)
            | n < nd = (map fst . filter ((==5).snd) . M.toList $ m) ++ go nd M.empty xs'
            | otherwise = go n (M.insertWith (+) k 1 m) xs

whichCubeRoot n = snd . head . filter ((==n).snd) $ vCubeSeq

whichCube (c,t) = map snd . filter ((==(c,t)).fst) . takeWhile (\((cnt, _),_) -> cnt <= c) $ tagedSeq

euler = head . map whichCubeRoot . concatMap whichCube . seek $ 13
