module Euler.P021 where
import qualified Data.IntMap as M
import Control.Monad

type D = M.IntMap Int

allDivisors n = 1:go [2..n] n
   where go []     n = []
         go (x:xs) n
            | d < 2     = []
            | r == 0    = let p = n `quot` x
                          in  case x `compare` p of
                                LT -> x:p:go xs n
                                EQ -> [x]
                                GT -> []
            | otherwise = go xs n
            where (d,r) = n `quotRem` x

divisorSum n = let v = (sum . allDivisors) n in v `seq` v

allDivisorSum = M.fromList $ map (\x->(x, divisorSum x)) [2..20000]

isAmicableNumbers k =
    case isAmicableNumbers' k of
        Nothing -> False
        Just v  -> v

isAmicableNumbers' k = do
     v <- M.lookup k allDivisorSum
     v' <- M.lookup v allDivisorSum
     return (k /= v && v' == k)

allAmicableNumbers = sum . filter isAmicableNumbers $ [2..9999]

