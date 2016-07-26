module Euler.P113 where

import qualified Data.Map.Strict as M
import Data.List (foldl')

data Direction = Desc | Inc deriving (Show,Eq,Ord)
type D = M.Map (Char, Integer, Direction) Integer

initDict :: D
initDict = M.fromList . concat $ [[((x,1,Inc), 1), ((x,1,Desc),1)] | x <- ['0'..'9']]

update :: D -> Integer -> D
update dict k = foldr (rupdate k) dict ['0'..'9']

rupdate k ch d0 = d2
  where  ppos = k - 1
         incc = sum [d0 M.! (x,ppos, Inc) | x <- ['0'..ch]]
         d1   = M.insert (ch, k, Inc) incc d0
         desc = sum [d1 M.! (x,ppos,Desc) | x <- [ch..'9']]
         d2   = M.insert (ch, k, Desc) desc d1

pseq n = foldl' update initDict [2..n]

allNonLeapCnt n = sum [countOnKDigits k | k <- [1..n]]
    where d = pseq n
          countOnKDigits k = incCnt + decCnt - 9
            where incCnt = sum [d M.! (x,k,Inc) | x <- ['1'..'9']]
                  decCnt = sum [d M.! (x,k,Desc)| x <- ['1'..'9']]
