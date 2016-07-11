module Euler.P004 where
{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.
-}

calc = maximum $ [allPalind x | x <- [999, 998..100]]

allPalind l = head . filter isPalind $ [l*x | x <- [999,998..l] ] ++ [0]

isPalind n = r == reverse r
   where r = show n

