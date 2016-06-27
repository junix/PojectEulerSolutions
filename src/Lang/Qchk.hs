{-# OPTIONS_GHC -Wall #-}
module Lang.Qchk where

prop_numElements_merge :: [Integer] -> [Integer] -> Bool
prop_numElements_merge xs ys
  = length xs + length ys == length (merge1 xs ys)

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys)
  | x < y     = x : merge1 xs ys
  | otherwise = y : merge1 xs ys
merge1 _      _      = []


aeqb :: Int -> Int -> Bool
aeqb a b = a /= b

