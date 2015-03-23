{-
    There are two sorted arrays A and B of size m and n respectively. 
    Find the median of the two sorted arrays. 
    The overall run time complexity should be O(log (m+n)).
-}

import Control.Monad.State

-- state format (maxVal,kth)
type CalcState = (Maybe Integer,Integer)

step v = state $ \(maxv,kth) -> 
  ((),(case kth `compare` 1 of
         LT -> (maxv,  kth)
         GT -> (maxv,  kth-1)
         EQ -> (Just v,0))) 

eat []     [] = state $ \s -> ((),s)
eat [] (x:xs) = do { step x; eat [] xs }
eat (x:xs) [] = do { step x; eat xs [] }
eat l@(xl:xsl) r@(xr:xsr) = 
  if xl < xr then 
    do { step xl; eat xsl r }
  else 
    do { step xr; eat l xsr }

calc l r = 
 let kth = (length l + length r + 1) `div` 2
     allState = eat l r
 in  fst $ execState allState (Nothing,kth)
