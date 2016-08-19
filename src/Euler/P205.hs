module Euler.P205 where
import System.Random
import Data.Ratio
import Data.List (groupBy,sort)
import Data.Function (on)
import Text.Printf(printf)

-- simulation method
rseq :: Int -> Integer -> [Integer]
rseq seed range = go . mkStdGen $ seed
    where go gen = let (r,g) = randomR (1,range) gen
                   in r : go g

groupSeq seed range groupSize = go xs
    where xs = rseq seed range
          go xs = let (hs,ts) = splitAt groupSize xs
                  in sum hs : go ts

pyraSeq seed = groupSeq seed 4 9
cubeSeq seed = groupSeq seed 6 6

matchSeq seed n = length . filter (==True). take n $ zipWith (>) (pyraSeq seed) (cubeSeq seed)

euler = matchSeq 1234 (10^7)

-- math solution
baseProb n = map (\x -> (x,1%n)) [1..n]

merge xs = (x, p)
    where x = fst . head $ xs
          p = sum . map snd $ xs

uniq = map merge . groupBy ((==) `on` fst) . sort

dice 1 ps bs = ps
dice n ps bs = dice (n-1) ps' bs
    where ps' = uniq [ (a+b, pa*pb) | (a, pa) <- ps, (b, pb) <- bs ]

pyraProbs = dice 9 (baseProb 4) (baseProb 4)
cubeProbs = dice 6 (baseProb 6) (baseProb 6)

euler0 = print (fromRational prob)
   where prob = sum [ pp*cp | (pv,pp) <- pyraProbs, (cv,cp) <- cubeProbs, pv > cv]