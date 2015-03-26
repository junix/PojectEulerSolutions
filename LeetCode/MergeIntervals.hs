{-
  Given a collection of intervals, merge all overlapping intervals.
 
  For example,
  Given (1,3),(2,6),(8,10),(15,18),
  return (1,6),(8,10),(15,18).
-}

import Data.List

isOverLap (x1,y1) (x2,y2) = not (y1 < x2 || y2 < x1)

mergeOverLap (x1,y1) (x2,y2) = (min x1 x2,max y1 y2)

merge' :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer,Integer)]
merge' e [] = [e]
merge' e (x:xs)
  | isOverLap e x = merge' (mergeOverLap e x) xs
  | otherwise     = x : merge' e xs

calc = Data.List.sort . foldr merge' []

