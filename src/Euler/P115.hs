module Euler.P115 where

import qualified Data.Map.Strict as M
import Control.Monad.State

type S = M.Map Integer Integer

f :: Integer -> Integer -> State S Integer
f m n
  | n < m  = state $ \s -> (1, M.insert n 1 s)
  | otherwise = state $ \s ->
            case M.lookup n s of
                Just v ->
                     (v, s)
                Nothing ->
                    let subc = (n-1) : map (n-1-) [m..n]
                        (xs, s') = runState (mapM (f m) subc) s
                    in (sum xs, M.insert n (sum xs) s')

calc n = fst $ runState (f 50 n) M.empty

euler = head . filter ((>10^6).calc) $ [50..]
