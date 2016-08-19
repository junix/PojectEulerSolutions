module Euler.P205 where
import Data.Ratio
import Data.List (groupBy,sort)
import Data.Function (on)

basicProbs n = map (\x -> (x,1%n)) [1..n]

simplify = map merge . groupBy ((==) `on` fst) . sort
    where merge xs = (x, p)
              where x = fst . head    $ xs
                    p = sum . map snd $ xs

play 1 ps bs = ps
play n ps bs = play (n-1) (simplify ps') bs
    where ps' = [ (a+b, pa*pb) | (a, pa) <- ps, (b, pb) <- bs ]

pyraProbs = let ps = basicProbs 4 in play 9 ps ps
cubeProbs = let ps = basicProbs 6 in play 6 ps ps

euler = round . (*10^7) . fromRational . sum $ probs
   where probs = [ pp*cp | (pv,pp) <- pyraProbs, (cv,cp) <- cubeProbs, pv > cv ]