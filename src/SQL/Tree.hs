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

movs children level = deps children (level-1)

addBars :: [String] -> String
addBars =  ('|':) . (++"|") . concat . intersperse "|"

bold xs = if length xs <= 5 then "`"++xs++"`" else xs

boldFmtTab :: [[Integer]] -> [[Bool]]
boldFmtTab xxs = [ map (<10^5) xs | xs <- xxs]

boldTab :: [[Bool]] -> [[Integer]] -> [[String]]
boldTab xxs yys = [ map render $ zip xs ys | (xs,ys) <- zip xxs yys]
    where render (False, x) = show x
          render (True,  x) = ('`':show x) ++ "`"

fmt = boldFmtTab [ c : map (deps c) [1..10] | c <- [2,3,4,5,6,7,8,9,10,15,20,50,100]]

stats f = map addBars $ (titles : delims : rstats)
    where titles = "sub deps" : map (("level "++).show) [1..10]
          delims = replicate 11 " :--: "
          stats  = [ c : map (f c) [1..10] | c <- [2,3,4,5,6,7,8,9,10,15,20,50,100]]
          rstats = boldTab fmt stats

main = do
    mapM_ putStrLn (stats deps)
    putStrLn ""
    mapM_ putStrLn (stats paths)