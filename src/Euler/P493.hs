module Euler.P493 where
import Data.Ratio
import Data.List((\\),sort,groupBy)
import Data.Function(on)
import Control.Lens

data Ball = Ball Integer deriving (Ord,Show,Eq)
type Box = [Ball]

initBox = concatMap (replicate 10 . Ball) [0..6]

len :: Box -> Integer
len = toInteger.length

nextProb [] = []
nextProb xs = uniq . zip xs . repeat . (1 %) . len $ xs

uniq =  map (\(ys@((y,_):_)) -> (y, sum.map snd $ ys)) . groupBy ((==) `on` fst) . sort

play pss = uniq [ (sort (b:xs),prob*p) | (xs, prob) <- pss, (b, p) <- nextProb . (initBox \\) $ xs ]
    where uniq =  map (\(ys@((y,_):_)) -> (y, sum.map snd $ ys)) . groupBy ((==) `on` fst) . sort

playN xss n = (!!n) . iterate play $ xss

nextShape :: [Integer] -> [[Integer]]
nextShape xs
    | len < 7 = (1:xs) : incs
    | otherwise = incs
    where len = length xs
          xs' = zip [0..] xs
          incs = [ xs & ix i +~ 1 | (i,x) <- xs', x < 10]
