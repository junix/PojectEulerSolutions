module Main where
import Math.NumberTheory.Primes
import qualified  Data.Map as M

type Dict = M.Map Integer Integer

pseq = reverse $ zip s (dropWhile (<=5) primes)
    where s =  dropWhile (<5). takeWhile (<=10^6) $ primes

neighbourDict :: Dict
neighbourDict = M.fromList pseq

con (a,b) = go 1 rb
    where r = rem10 a
          rb = r `rem` b
          sr = b-a
          go c y
            | y == sr = c*r  + a
            | otherwise = go (c+1) ((y+rb) `rem` b)

rem10 :: Integer -> Integer
rem10 = (10^) . ceiling . logBase 10 . fromInteger

cont (a,b) = go 1 rb
    where r = rem10 a
          rb = b `rem` r
          go c y = if y `rem` r == a
                   then c*b
                   else go (c+1) y'
              where y' = (y + rb) `rem` r

euler = map con . reverse $ pseq

main = print euler

digits 0 = []
digits n = r : digits q
    where (q,r) = n `quotRem` 10

tailInt :: Integer -> [Integer]
tailInt = go . digits
    where go []     = []
          go (0:xs) = map (10*) (go xs)
          go (x:xs) = x : (map ((+x).(10*)) . go) xs


try :: Integer -> Dict -> Dict -> (Dict,Dict)
try n s d = go s d ts
    where ts = tailInt n
          go s' d' [] = (s',d')
          go s' d' (x:xs) = case M.lookup x d' of
                                Nothing -> go s' d' xs
                                Just v ->  if n `rem` v == 0
                                           then go (M.insert x n s') (M.delete x d') xs
                                           else go s' d' xs

pps = go M.empty neighbourDict  (filter odd [10..])
    where go solveDict nonSolveDict (x:xs)
              | M.null nonSolveDict = [M.size solveDict]
              | otherwise = let (s,d) = try x solveDict nonSolveDict
                            in if M.size s /= M.size solveDict
                               then M.size s : go s d xs
                               else go s d xs
