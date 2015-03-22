{-
Given an array of integers, find two numbers such that they add up to a specific target number.

The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2. Please note that your returned answers (both index1 and index2) are not zero-based.

You may assume that each input would have exactly one solution.

Input: numbers={2, 7, 11, 15}, target=9
Output: index1=1, index2=2 
-}

import Data.List (filter,nub,sort,reverse,map)
import Control.Monad.State
import Data.Map

--twoSum :: [Int] -> Int -> [(Integer,Integer)]
twoSum xs t = let 
                xsWithIndexes = xs `zip` [1..]
              in 
                [(ai,bi) | (a,ai) <- xsWithIndexes
                         , (b,bi) <- xsWithIndexes
                         , a+b==t
                         , ai < bi]

twoSum' :: [Integer] -> Integer -> [(Integer,Integer)]
twoSum' xs t = let 
                xsi = sort $ zip xs [1..]
                allPairs = merge xsi (reverse xsi)
               in
                 sort.nub.(Data.List.filter (\(x,y) -> x /= y)).(Data.List.map (\(x,y)->if (x < y) then (x,y) else (y,x) )) $ allPairs
               where
                merge :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
                merge [] _ = []
                merge  _ [] = []
                merge xsl@(x1:xs1) xsr@(x2:xs2) = case (fst x1+fst x2) `compare` t of
                                             LT ->  merge xs1 xsr
                                             GT ->  merge xsl xs2
                                             EQ ->  let
                                              split v l = span (\(a,_)->a==fst v) l
                                              (eql,leftl) = split x1 xsl
                                              (eqr,leftr) = split x2 xsr
                                              in
                                                 [(l,updateAll) | (_,l) <- eql,(_,updateAll)<-eqr] ++ merge leftl leftr
                    
                                                    


-- 使用State Monad来实现TwoSum算法
type IndexMap = Map Integer [Integer]

ups = Data.Map.insert

updateOne :: Integer -> (Integer,Integer) -> State IndexMap [(Integer,Integer)]
updateOne t (v,i) = state $ \dict->case Data.Map.lookup (t-v) dict of 
                                         Just inds -> (zip inds (repeat i), dict)
                                         _ -> case Data.Map.lookup v dict of 
                                                Just ids -> ([],ups v (i:ids) dict)
                                                _ -> ([],ups v [i] dict)

updateAll :: Integer -> [(Integer,Integer)]  -> State IndexMap [(Integer,Integer)]
updateAll t [] = do {return []}
updateAll t (x:xs) = do
    v0 <- updateOne t x
    v1 <- updateAll t xs
    return (v0 ++ v1)

twoSum'' xs t = let s = updateAll t $ sort $ xs `zip` [1..] 
                    (pairs,_) = runState s Data.Map.empty
                    normal = Data.List.map (\(a,b) -> if a <= b then (a,b) else (b,a)) pairs
                in
                    Data.List.sort normal
