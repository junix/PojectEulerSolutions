module Euler.P122 where

import qualified Data.Map.Lazy as M

initSteps n = M.fromList $ map (\x->(x,(x-1,[1]))) [1..n]

iter m n = go m [1..n]
    where go m [] = m
          go m xs = foldl updateBy m xs

          updateBy m x = foldl
                (\d step ->
                    if x+step > n
                    then d
                    else M.insertWith select (x+step) (c+1, c:steps) d) m (c:steps)
              where (c,steps) = m M.! x

          select (c1,steps1) (c2, steps2)
              | c1 < c2   = (c1,steps1)
              | c1 == c2  && length steps1 <= length steps2 = (c2,steps2)
              | c1 == c2  && length steps1 > length steps2 = (c1,steps1)
              | otherwise = (c2,steps2)
