module Euler.P065 where
import Data.Ratio

convs :: [Integer]
convs = concatMap (\x -> [1,2*x,1]) $ [1..]

calc n = 2 + (foldr update 0 . take n $ convs)

update k acc = d % (k*d + n)
    where n = numerator   acc
          d = denominator acc

euler = sum . map read . map (:[]). show . numerator . calc $ 99
