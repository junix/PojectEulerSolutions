module Euler.P127 where

import Data.List (intersect, (\\))
import Math.NumberTheory.Primes
import Data.Map.Lazy(fromList, empty,insert,lookup, (!), toList)

data Fact = Fact { fseq      :: [Integer]
                 , allOneExp :: Bool
                 , rad       :: Integer
                 , v         :: Integer
                 } deriving Show

factors :: Integer -> Fact
factors n = Fact { fseq      = map fst fs
                 , allOneExp = (maximum . (0:) . map snd $ fs) == 1
                 , rad       = product . map fst $ fs
                 , v         = n
             }
   where fs = factorise n

factorsDict =  fromList $ zip [1..] . map factors $ [1..120000]

probSeq = map snd . filter (allOneExp.snd) . toList $ factorsDict

abcHitFor c = if allOneExp fc then [] else go [1..ceil]
     where fc = factorsDict ! c
           ceil = c `quot` 2
           go [] = []
           go (a:as)
               | rac > c || rabc >= c  || not allGcds = go as
               | otherwise = c : go as
               where b = c - a
                     fa = factorsDict ! a
                     fb = factorsDict ! b
                     rac = rad fa * rad fc
                     rabc = rad fa * rad fb * rad fc
                     allGcds = gcd1 fa fb && gcd1 fa fc && gcd1 fb fc

gcd1 fx fy = null (fseq fx `intersect` fseq fy)


euler n = concatMap abcHitFor [2..n]

e = euler 120000

p127 = sum e
