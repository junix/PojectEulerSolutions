module Euler.P081 where

import Data.List.Split (wordsBy)
import qualified Data.Map as M
import Control.Monad

type Pos = (Integer,Integer)
type Dict = M.Map Pos Integer

readMaxtrix :: IO [[Integer]]
readMaxtrix = readFile "./p081_matrix.txt" >>= return . line2maxtrix

line2maxtrix :: String -> [[Integer]]
line2maxtrix = map (map read . wordsBy (==',')) . lines

neigh :: Pos -> [Pos]
neigh (x, y) = [(a,b) | (a,b) <- [(x-1,y), (x,y-1)], a >= 0, b >= 0]

get m (x, y) = m !! fromInteger y !!  fromInteger x

orderSeq :: Integer -> [Pos]
orderSeq len = go ((len-1)*2)
    where go 0 = [(0,0)]
          go n = go (n-1) ++ [(x,y) |x <- [0..n], let y = n - x, x < len, y < len]

euler maxtrix = foldl update M.empty (orderSeq len)
    where len = fromIntegral . length $ maxtrix
          update :: Dict -> Pos -> Dict
          update dict pos = M.insert pos maxv dict
            where ns = map (dict M.!) . neigh $ pos
                  my = get maxtrix pos
                  maxv = if null ns then my else minimum ns + my

main = do
    maxtrix <- readMaxtrix
    print (euler maxtrix M.! (79,79))
