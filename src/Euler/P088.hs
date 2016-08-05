module Euler.P088 where
import Math.NumberTheory.Primes
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Array

data Div = Div { cnt :: Integer
               , acc :: Integer
               } deriving(Eq,Ord,Show)

join x (Div{cnt = c, acc = a}) = Div {cnt = c + 1, acc = a + x}
merge (Div{cnt = c1, acc = a1}) (Div{cnt = c2, acc = a2}) = Div {cnt = c1+c2, acc = a1+a2}

divs n = [e | e <- ds, e > 1, e <= sq]
    where sq = truncate . sqrt . fromInteger $ n
          ds = S.toList .  divisors $ n

ccnt n (Div{cnt = c, acc = a}) = c + (n-a)

psn :: M.Map Integer [Div] -> Integer -> M.Map Integer [Div]
psn dict n = M.insert n (nub . concatMap go $ bs) dict
    where bs = [(x,y)| x <- divs n, let y = n `quot` x]
          go (x,y)
            | isPrime x && isPrime y = [base]
            | isPrime x = base : joinx
            | isPrime y = base : joiny
            | otherwise = base : [merge a b | a <-  seek x, b <- seek y] ++ joinx ++ joiny
            where base = Div {cnt = 2, acc = x + y}
                  seek x = dict M.! x
                  joiny = (map (join y) . seek) x
                  joinx = (map (join x) . seek) y

pp = map (\(n,divs) -> (n, map (ccnt n) divs)) . filter (not.null.snd) . zip [4..] . go 4 $ M.empty
    where go n d = d' M.! n : go (n+1) d'
            where d' = (psn d n)

euler = go pp M.empty
    where go ((v,cs):xs) d = d' : go xs d'
              where d' = foldl update d cs
                    update dict c = if M.member c dict then dict else M.insert c v dict

p088 n = sum . S.toList . S.fromList . map ((euler !! 15000) M.!) $  [2..n]

