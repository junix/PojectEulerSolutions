module Euler.P127 where

import Data.List (intersect, (\\))
import Math.NumberTheory.Primes
import Data.Map.Lazy(fromList, empty,insert,lookup, (!))

data Fact = Fact { fseq      :: [Integer]
                 , allOneExp :: Bool
                 , rad       :: Integer
                 } deriving Show

factors :: Integer -> Fact
factors n = Fact { fseq = map fst f
                 , allOneExp = (maximum . (0:) .map snd $ f) == 1
                 , rad = product . map fst $ f
             }
   where f = factorise n

factorsDict =  fromList $ map (\x-> (x, factors x)) [1..120000]

abcHitFor c = if allOneExp fc
              then []
              else go [1..ceil]
              --go [1..ceil]
     where
           fc = factorsDict ! c
           ceil = let half = c `quot` 2 in if even c then half - 1 else half
           go [] = []
           go (a:as) = if rad fa * rad fc > c
                       then go as
                       else if rad fa * rad fb * rad fc > c
                            then go as
                            else if (gcd1 fa fb && gcd1 fa fc && gcd1 fb fc)
                                 then [c]
                                 else go as
               where b = c - a
                     fa = factorsDict ! a
                     fb = factorsDict ! b

gcd1 fx fy = null (fseq fx `intersect` fseq fy)


euler n = concatMap abcHitFor $ [2..n]
