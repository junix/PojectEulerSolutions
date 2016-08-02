module Euler.P114 where
import qualified Data.Map.Strict as M
import Control.Monad.State

type S = M.Map Integer Integer

c :: Integer -> State S Integer
c n
  | n < 3  = state $ \s -> (1, M.insert n 1 s)
  | n == 3 = state $ \s -> (2, M.insert n 2 s)
  | otherwise = state $ \s ->
            case M.lookup n s of
                Just v ->
                     (v, s)
                Nothing ->
                    let subc = (n-1) : map (n-1-) [3..n]
                        (xs, s') = runState (mapM c subc) s
                    in (sum xs, M.insert n (sum xs) s')

euler = fst $ runState (c 50) M.empty

tabulate bounds f = array bounds [(i,f i) | i <- range bounds]
dp bounds f = (memo!) where memo = tabulate bounds (f (memo!))

p114 n = dp (-1,n) f n where
  f rec x | x <= 0 = 1
          | otherwise = sum [rec(x-a) | a <-1:[4..x+1],x>a-2]

main = print (p114 50)
