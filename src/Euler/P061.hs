module Euler.P061 where

f 3 = \x -> x*(x+1) `quot` 2
f 4 = (^2)
f 5 = \x->x*(3*x - 1) `quot` 2
f 6 = \x->x*(2*x - 1)
f 7 = \x->x*(5*x - 3) `quot` 2
f 8 = \x->x*(3*x - 2)

p n = dropWhile (<10^3) . takeWhile (<10^4) . map (f n) $ [1..]

sp n = n `quotRem` 100

allSeq = map (map (`quotRem` 100) . p) [3..8]

seek = sum . map (\(x,y) -> x*100+y) . head . filter isChained . concatMap (chain left) $ (head allSeq)
    where left = tail allSeq

seekNext [] (h,t) = []
seekNext xss (h,t) = go xss []
    where go [] acc = []
          go (xs:xss) acc = [((x,y), acc ++ xss)|(x,y) <- xs, x == t] ++ go xss (xs:acc)

chain :: [[(Integer, Integer)]] -> (Integer, Integer) ->  [[(Integer, Integer)]]
chain [] x = [[x]]
chain xss x = concat [ map (x:) (chain yss (h,t))  | ((h,t), yss)<- seekNext xss x]

isChained xs = h == t
    where h = fst.head $ xs
          t = snd.last $ xs
