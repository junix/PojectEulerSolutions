module Euler.P024 where
import Data.List

perms :: String -> [String]
perms []  = []
perms xs = xs : (case nextPerm xs of
                    Nothing -> []
                    Just v  -> perms v)

nextPerm xs = case remain of
        []           -> Nothing
        ((a,b):ts')  ->
            let (v,y:ys) = break (>b) (map fst inc ++ [a])
            in Just ((reverse.map snd $ ts') ++ [y] ++ v ++ b:ys)
    where
        rev = reverse xs
        (inc,remain) = break (\(x,y) -> x > y) $ zip rev (tail rev)

euler24 :: Integer
euler24 = read . head . drop  999999 $ perms "0123456789"

lexPerms [x] = [[x]]
lexPerms xs = concatMap (\x -> map (x:) . lexPerms $ delete x xs) xs

solution = (lexPerms [0..9]) !! 999999

