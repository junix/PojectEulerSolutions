module Main ( main ) where
import Control.Parallel.Strategies
import Math.NumberTheory.Primes
import Data.List (sort,delete)

pseq = primes

perms :: String -> [String]
perms []  = []
perms xs = xs : (case nextPerm xs of
                    Nothing -> []
                    Just v  -> perms v)

nextPerm xs = case remain of
        []           -> Nothing
        ((a,b):ts')  ->
            let (v,y:ys) = break (>b) (map fst inc ++ [a])
            in Just ((reverse.map snd $ ts') ++ [y] ++ v ++ b:ys)
    where
        rev = reverse xs
        (inc,remain) = break (\(x,y) -> x > y) $ zip rev (tail rev)

nextSeq :: Maybe String -> Maybe String
nextSeq Nothing = Nothing
nextSeq (Just xs) = case nx of
                Nothing -> Nothing
                Just ('0':cs) -> nextSeq (Just ('0':cs))
                Just v -> Just v
    where nx = nextPerm xs

startSeq k c rs = if h == '0' then nextSeq (Just beg) else (Just beg)
    where beg = sort (replicate k c ++ rs)
          (h:_) = beg

dseq :: Int -> Char -> String -> [Integer]
dseq k c rs = go (startSeq k c rs)
    where go Nothing = []
          go v'@(Just v) = read v : go (nextSeq v')

maxLenSeq n c = head [ ps | k <- [n-1,n-2..1]
                        , let ps = filter isPrime . concatMap (dseq k c) $ select (n-k) (delete c chset)
                        , not . null $ ps ]
    where chset = ['0'..'9']

allSeqs = concatMap (maxLenSeq 10) ['0'..'9']

select 0 xs = [[]]
select n [] = []
select n (c:cs) = [ replicate k c ++ r  | k <- [0..n], r <- select (n-k) cs]
