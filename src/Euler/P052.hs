module Euler.P052 where

import Data.List (sort)

permed x y = go x == go y
    where go = sort.show

allPermed x = all (permed x1) . map (x*) $ [3..6]
    where x1 = x*2

euler = filter allPermed $ [1..]

