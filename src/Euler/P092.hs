module Euler.P092 where
import qualified Data.Map.Strict as M

next n
    | n == 89 = 89
    | n == 1  = 1
    | otherwise = next (squared n)

squared n = go n 0
   where go n acc
           | n < 10 = acc + n^2
           | otherwise = go q (acc+r^2)
           where (q,r) = n `quotRem` 10

euler = length . filter (==89) . map (\x -> d M.! (squared x)) $ [1..10000000 - 1]
    where d = foldl update M.empty [1..1000]

update m n = case M.lookup sq m of
                Nothing -> M.insert n (next n) m
                Just v  -> M.insert n v m
    where sq = squared n