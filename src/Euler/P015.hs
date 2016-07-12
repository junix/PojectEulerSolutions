module Euler.P015 where
import Data.Array


import Control.Monad.State
import qualified Data.Map.Strict as M

type S = M.Map (Int, Int) Int

routes :: Int -> Int -> State S Int
routes 0 w = state $ \s -> (1, M.insert (0, w) 1 s)
routes l 0 = state $ \s -> (1, M.insert (l, 0) 1 s)
routes l w = state $ \s ->
    case M.lookup (l, w) s of
        Just c  -> (c, s)
        Nothing ->
            let (c', s') = runState (do {a <- routes (l-1) w
                                        ;b <- routes l (w-1)
                                        ; return (a+b)
                                        }) s
            in (c', M.insert (l,w) c' s')

calc n = fst $ runState (routes n n) M.empty
euler15 = calc 20


tabulate bounds f = array bounds [(i,f i) | i <- range bounds]
dp bounds f = (memo!) where memo = tabulate bounds (f (memo!))

p15 n = dp ((0,0),(n,n)) f (n,n)
    where f rec (x,y)
             | x == 0 || y == 0 = 1
             | otherwise        = rec (x-1,y) + rec (x,y-1)