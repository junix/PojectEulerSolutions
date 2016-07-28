module Euler.P082 where
import Data.Array (Array(..), (!), listArray)
import Data.STRef
import Control.Monad.ST
import Data.Array.ST
import Data.List.Split (wordsBy)

test = concat $
    [ [131 ,673  ,234  , 103 , 18 ]
    , [201 , 96  , 342 , 965 , 150]
    , [630 , 803 , 746 , 422 , 111]
    , [537 , 699 , 497 , 121 , 956]
    , [805 , 732 , 524 , 37  , 331]]

initm xs = newListArray (0,l) xs' :: ST s (STArray s Integer (Integer,Integer))
   where l = toInteger $ length xs - 1
         xs' = map (\x -> (10^10,x)) xs

m2v l x y = y*l + x

nei l x y = filter (\(a,b) -> iN a && iN b) [(x-1,y), (x,y-1), (x,y+1)]
    where iN v = v >= 0 && v < l

init1 :: Integer
      -> Integer
      -> Integer
      -> STArray s Integer (Integer,Integer)
      -> ST s (STArray s Integer (Integer,Integer))
init1 l s e m = do
    if s >= e
       then return m
       else do
               let p = s*l
               (a,b) <- readArray m p
               writeArray m p (b,b)
               init1 l (s+1) e m

update :: Integer
       -> STArray s Integer (Integer,Integer)
       -> ST s (STArray s Integer (Integer,Integer))
update l m = go m 0 0 False
   where
         go :: STArray s Integer (Integer,Integer)
            -> Integer
            -> Integer
            -> Bool
            -> ST s (STArray s Integer (Integer, Integer))
         go m x y b
            | x >= l && b == False = return m
            | x >= l && b = go m 0 0 False
            | y >= l = go m (x+1) 0 b
            | otherwise = do
                     maxv <- mapM (\(a,b) -> readArray m (m2v l a b)) $ nei l x y
                     (c,len) <- readArray m (m2v l x y)
                     let newv = minimum (c:map ((len+).fst) maxv)
                     writeArray m (m2v l x y) (newv, len)
                     go m x (y+1) (b || newv /= c)

euler xs = minimum . map (es!!) $ lastCols
    where len = truncate . sqrt . fromInteger . toInteger . length $ xs
          lastCol = len - 1
          es = runST $ initm xs>>= init1 len 0 len>>= update  len>>= getElems
          lastCols = map fromInteger .  map (m2v len lastCol) $ [0..lastCol]

main = do
    c <- readFile "p082_matrix.txt"
    let xs = (map read . concatMap (wordsBy (==',')) . lines $ c) :: [Integer]
    print $ euler xs
