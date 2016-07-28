module Euler.P083 where
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

nei l x y = filter (\(a,b) -> iN a && iN b) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    where iN v = v >= 0 && v < l

initLT :: STArray s Integer (Integer,Integer) -> ST s (STArray s Integer (Integer,Integer))
initLT m = do{ (a,b) <- readArray m 0
               ; writeArray m 0 (b,b)
               ; return m }

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

euler xs = last es
    where len = truncate . sqrt . fromInteger . toInteger . length $ xs
          lastCol = len - 1
          es = runST $ initm xs>>= initLT >>= update len>>= getElems

main = do
    c <- readFile "p083_matrix.txt"
    let xs = (map read . concatMap (wordsBy (==',')) . lines $ c) :: [Integer]
    print $ euler xs
