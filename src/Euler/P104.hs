{-# LANGUAGE BangPatterns #-}
module P104 where
import Data.List (nub)

{-
Project Euler Problem 104
=========================

The Fibonacci sequence is defined by the recurrence relation:

  F[n] = F[n[1]] + F[n[2]], where F[1] = 1 and F[2] = 1.

It turns out that F[541], which contains 113 digits, is the first
Fibonacci number for which the last nine digits are 1-9 pandigital
(contain all the digits 1 to 9, but not necessarily in order). And
F[2749], which contains 575 digits, is the first Fibonacci number for
which the first nine digits are 1-9 pandigital.

Given that F[k] is the first Fibonacci number for which the first nine
digits AND the last nine digits are 1-9 pandigital, find k.
-}

data Fib = Fib { v     :: Integer
               , index :: Integer
               , tl    :: Integer
               }

fibs =  f1 : f2 : go f1 f2
    where f1 = Fib{v = 1, index = 1, tl = 1}
          f2 = Fib{v = 1, index = 2, tl = 1}
          go (Fib{v = v1, tl = t1})  s@(Fib{v = v2, index = i2, tl = t2}) = f : go s f
              where f = Fib { v = v1 + v2, index = i2 + 1 , tl = (t1+t2) `rem` (10^10) }

last10pand n = go 1 n []
    where go 10 _ acc = True
          go c 0 acc  = False
          go c n acc  = r /= 0 &&  r `notElem` acc &&  go (c+1) q (r:acc)
              where (q,r) = n `quotRem` 10

isPandPrefix n = '0' `notElem` hd &&  (length . nub) hd == 9
    where hd = take 9 . show $ n

euler = index . head . filter (isPandPrefix.v) . filter (last10pand.tl) $ fibs
