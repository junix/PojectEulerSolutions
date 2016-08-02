module SQL.Tree where
import Data.List (intersect,intersperse)

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

addBars :: [String] -> String
addBars xs =  ("|"++) . (++"|") . concat . intersperse "|" $ xs

printDeps = map addBars $ (titles : delims : stats)
    where titles = "sub deps" : map (("level "++).show) [1..10]
          delims = replicate 11 " :--: "
          stats = [ show c : map (bold.show.deps c) [1..10] | c <- [2,3,4,5,6,7,8,9,10,15,20,50,100]]
          bold xs = if length xs <= 5 then "`"++xs++"`" else xs

main = do
    mapM_ putStrLn printDeps