module SQL.Tree where

deps _ 0 = 1
deps children level = ancestors + parents * children
    where plevel    = level - 1
          parents   = children^plevel
          ancestors = deps children plevel

paths children 0     = 0
paths children level = ppaths + cpaths
    where plevel = level - 1
          cpaths = deps children level*level
          ppaths = paths children plevel
