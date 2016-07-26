module Euler.P113 where

import qualified Data.Map.Strict as M
import Data.List (foldl')

data Direction = Desc | Inc deriving (Show,Eq,Ord)
type D = M.Map (Char, Integer, Direction) Integer

initDict :: D
initDict = M.fromList . concatMap (\x -> [((x,1,Inc), 1), ((x,1,Desc),1)]) $ ['0'..'9']

update :: D -> Integer -> D
update dict k = foldr (rupdate k) dict ['0'..'9']

rupdate k ch d0 = d2
  where  ppos = k - 1
         incc = sum . map (d0 M.!) . map (\x -> (x,ppos, Inc)) $ ['0'..ch]
         d1   = M.insert (ch, k, Inc) incc d0
         desc = sum . map (d1 M.!) . map (\x -> (x,ppos,Desc)) $ [ch..'9']
         d2   = M.insert (ch, k, Desc) desc d1

pseq n = foldl' update initDict [2..n]

allNonLeapCnt n = sum . map exactNonLeapCnt $ [1..n]
    where d = pseq n
          exactNonLeapCnt k = (sum . map (d M.!) . map (\x -> (x,k,Inc))  $ ['1'..'9']) +
                              (sum . map (d M.!) . map (\x -> (x,k,Desc)) $ ['1'..'9']) - 9
