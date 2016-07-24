module Euler.P099 where

import Data.List (maximumBy)
import Data.Function (on)

readExps :: IO [(Integer,Integer)]
readExps = do
    c <- readFile "p099_base_exp.txt"
    return (map (read . (\l -> "(" ++ l ++ ")")) . lines $ c)

maxExp xs = maximumBy (compare `on` snd) .  zip [1..] . map log10 $ xs
    where log10 (a,b) = (fromInteger b) * (log (fromInteger a))

main = do
    exps <- readExps
    print $ maxExp exps
