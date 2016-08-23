module Euler.P345 where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Function
import Data.List
import System.Random

count = 15

matrix = listArray ((1,1),(count,count)) $ concat
     [[7,53,183,439,863,497,383,563,79,973,287,63,343,169,583]
    ,[627,343,773,959,943,767,473,103,699,303,957,703,583,639,913]
    ,[447,283,463,29,23,487,463,993,119,883,327,493,423,159,743]
    ,[217,623,3,399,853,407,103,983,89,463,290,516,212,462,350]
    ,[960,376,682,962,300,780,486,502,912,800,250,346,172,812,350]
    ,[870,456,192,162,593,473,915,45,989,873,823,965,425,329,803]
    ,[973,965,905,919,133,673,665,235,509,613,673,815,165,992,326]
    ,[322,148,972,962,286,255,941,541,265,323,925,281,601,95,973]
    ,[445,721,11,525,473,65,511,164,138,672,18,428,154,448,848]
    ,[414,456,310,312,798,104,566,520,302,248,694,976,430,392,198]
    ,[184,829,373,181,631,101,969,613,840,740,778,458,284,760,390]
    ,[821,461,843,513,17,901,711,993,293,157,274,94,192,156,574]
    ,[34,124,4,878,450,476,712,914,838,669,875,299,823,329,699]
    ,[815,559,813,459,522,788,168,586,966,232,308,833,251,631,107]
    ,[813,883,451,509,615,77,281,613,459,205,380,274,302,35,805]]

-- from http://www.haskell.org/haskellwiki/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n =  newListArray (1,n)

new :: IO [Int]
new = shuffle [1..count]

mutate :: [Int] -> IO [Int]
mutate x = do
    a <- randomRIO (1-1,count-1-1)
    b <- randomRIO (a+1-1,count-1)
    let f (i,y) | i == a = x !! b
                | i == b = x !! a
                | otherwise = y
    return $ map f $ zip [1-1..]  x

fitness a = sum $ map (matrix !) $ zip [1..] a

evolve a = do
    let best = drop (length a `div` 10 * 9) . map snd . sortBy (compare `on` fst) $ zip (map fitness a) a
    let mutation x = forM [1..9] (const (mutate x))
    b <- mapM mutation best
    return $ best ++ concat b

evolving a = do
    print $ maximum (map fitness a)
    a' <- evolve a
    evolving a'

population = forM [1..1000] (const new)

main = evolving =<< population