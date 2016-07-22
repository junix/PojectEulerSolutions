module Euler.P080 where
import Data.Number.BigFloat
import Data.Char (ord)
import Data.List ((\\))

type Prec110 = PrecPlus20 (PrecPlus20 (PrecPlus20 Prec50))
type F = BigFloat Prec110

sqrt' :: Integer -> F
sqrt' n = sqrt $ fromInteger n

digitalSum n = sum .
               map (sub0 . ord) .
               take 100 .
               filter (/='.') .
               show .
               sqrt' $ n
   where sub0 = subtract (ord '0')

euler =  sum . map digitalSum $ [1..100] \\ (map (^2) [1..10])