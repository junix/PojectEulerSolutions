module Euler.P117 where
import qualified Data.Map.Strict as M
import Control.Monad.State

type S = M.Map Integer Integer

f :: Integer -> State S Integer
f n
  | n < 2  = state $ \s -> (1, M.insert n 1 s)
  | otherwise = state $ \s ->
            case M.lookup n s of
                Just v ->
                     (v, s)
                Nothing ->
                    let subc = filter (>=0) . map (n-) $ [1..4]
                        (xs, s') = runState (mapM f subc) s
                    in (sum xs, M.insert n (sum xs) s')

calc n = fst $ runState (f n) M.empty

euler = calc 50

