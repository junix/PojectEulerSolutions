module Euler.P214 where
import Math.NumberTheory.Primes
import Data.List (delete,union)
import Data.Ratio
import qualified Data.Map as M

phi n = numerator . (n % 1 *) . product $ [ 1 - 1%p | p <- ps]
    where ps = map fst . factorise $ n

ps = takeWhile (<4*10^7) primes

euler = sum . map snd . filter ((==25).fst) $ go ps (M.fromList [(1,1)])
    where
        go [] dict = []
        go (x:xs) dict = (c,x) : go xs d
            where run d p = case M.lookup p d of
                                Just c -> (c, d)
                                Nothing -> let  p' = phi p
                                                (c',d') = run d p'
                                           in (c'+1, M.insert p (c'+1) d')
                  (c, d) = run dict x


