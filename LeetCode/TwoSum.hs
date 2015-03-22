{-
Given an array of integers, find two numbers such that they add up to a specific target number.

The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2. Please note that your returned answers (both index1 and index2) are not zero-based.

You may assume that each input would have exactly one solution.

Input: numbers={2, 7, 11, 15}, target=9
Output: index1=1, index2=2 


-}

import Data.List (nub,sort,reverse)

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
                 sort.nub.filter (\(x,y) -> x /= y).map (\(x,y)->if (x < y) then (x,y) else (y,x) ) $ allPairs
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
                                                 [(l,r) | (_,l) <- eql,(_,r)<-eqr] ++ merge leftl leftr
                    
                                                    
