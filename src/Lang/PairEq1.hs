module Lang.PairEq1 where

import Lang.Pair (H)

instance Eq Pair where
    H a b == H c d = a ==b

