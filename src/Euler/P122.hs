module Euler.P122 where
import Data.List (nub)
import qualified  Data.Map as M
import qualified  Data.Set as S

stepOf = update (c+1) [1..200] dict
    where
        (c, dict) = go 0 (S.fromList [(1,[1])]) (M.fromList [(1,0)])
        go c xs dict
            | M.size dict >= 199 = (c,dict)
            | otherwise = go (c+1) xs' dict'
                where xs' = S.fromList [ (e', (S.toList . S.fromList . (e':)) $ ss)
                                       | (e,ss) <- S.toList xs
                                       , s <- ss
                                       , let e' = e + s
                                       , e' <= 200
                                       ]
                      vs  = map fst . S.toList $ xs'
                      dict' = update (c+1) vs dict

update n ks dict = foldr (\k d -> M.insertWith ignore k n d) dict ks
    where ignore = flip const

euler = sum . M.elems $ stepOf