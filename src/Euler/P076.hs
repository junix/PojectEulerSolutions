module Euler.P076 where

coef = map (\k -> (ex (k-1), k*(3*k-1) `quot` 2)) . concatMap (\x -> [x, -x]) $ [1..]
    where ex x | odd x = -1 | otherwise = 1

index n = takeWhile ((<n).snd) . map (\(x,y) -> (x, y-1)) $ coef


seqs = 1 : go 1 [1]
    where go n acc = cn : go (n+1) (cn:acc)
            where cn = sum . map (\(x,y) -> x*(acc!!y)) . index $ n

euler n = (seqs !! n) - 1