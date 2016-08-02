module SQL.Tree where

count :: Integer -> Integer -> Integer
count _ 0 = 1
count children level = ancestors + parents * children
    where plevel    = level - 1
          parents   = children ^plevel
          ancestors = count children plevel
