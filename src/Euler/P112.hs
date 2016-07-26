module Euler.P112 where

isLeap n
    | n < 100 = False
    | otherwise = go q0 r0 Nothing
    where (q0,r0) = n `quotRem` 10
          go 0 r _ = False
          go q r cmp
            | cmp == Nothing = go q' r' (if cmp' == Just EQ then Nothing else cmp')
            | otherwise = if cmp' == Just EQ
                          then go q' r' cmp
                          else if cmp == cmp'
                               then go q' r' cmp
                               else True
            where (q',r') = q `quotRem` 10
                  cmp' = Just (r `compare` r')

euler = head . filter (\(x,y) -> 100*x == y) $ go 2 (1,1)
    where go n (l,a) = (l,a):go (n+1) (if not (isLeap n) then (l+1,n) else (l,n))