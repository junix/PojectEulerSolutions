{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
module P107 where
import Data.List.Split (wordsBy)
import Data.Maybe
import qualified  Data.Map as M
import qualified  Data.Set as S
{-
The following undirected network consists of seven vertices and twelve
edges with a total weight of 243.

The same network can be represented by the matrix below.

               +-----------------------------------------+
               |      | A  | B  | C  | D  | E  | F  | G  |
               |------+----+----+----+----+----+----+----|
               | A    | -  | 16 | 12 | 21 | -  | -  | -  |
               |------+----+----+----+----+----+----+----|
               | B    | 16 | -  | -  | 17 | 20 | -  | -  |
               |------+----+----+----+----+----+----+----|
               | C    | 12 | -  | -  | 28 | -  | 31 | -  |
               |------+----+----+----+----+----+----+----|
               | D    | 21 | 17 | 28 | -  | 18 | 19 | 23 |
               |------+----+----+----+----+----+----+----|
               | E    | -  | 20 | -  | 18 | -  | -  | 11 |
               |------+----+----+----+----+----+----+----|
               | F    | -  | -  | 31 | 19 | -  | -  | 27 |
               |------+----+----+----+----+----+----+----|
               | G    | -  | -  | -  | 23 | 11 | 27 | -  |
               +-----------------------------------------+

However, it is possible to optimise the network by removing some edges and
still ensure that all points on the network remain connected. The network
which achieves the maximum saving is shown below. It has a weight of 93,
representing a saving of 243 93 = 150 from the original network.

Using network.txt, a 6K text file containing a network with forty vertices,
and given in matrix form, find the maximum saving which can be achieved by
removing redundant edges whilst ensuring that the network remains connected.
-}

type G = [[((Integer, Integer),Integer)]]
type NodeSet = S.Set Integer

main = do
    c <- readFile "p107_network.txt"
    print (isFullConnected . parseG . parseMat $ c)

parseMat :: String -> [[Maybe Integer]]
parseMat = map (map str2v.wordsBy (==',')). lines

str2v :: String -> Maybe Integer
str2v "-" = Nothing
str2v xs  = Just (read xs)

parseG :: [[Maybe Integer]] -> G
parseG xss = [ catMaybes [ (,) <$> Just (ir,ix) <*> x
                         | x <- xs
                         | ix <- [0..]
                         ]
             | xs <- xss
             | ir <- [0..]
             ]

conClosure :: Integer -> G -> NodeSet
conClosure x g = go (S.insert x . nei $ x) (S.fromList [x])
    where nei n = S.fromList . map (snd.fst) . (!! fromInteger n) $ g
          go ns cs
            | null diff = ns
            | otherwise = let v = head diff
                          in go (ns `S.union` nei v) (S.insert v cs)
            where diff = S.toList (ns S.\\ cs)

isFullConnected g = S.size (conClosure 0  g) == length g

deleteEdge x y g = [ [(e,w)| (e,w) <- rs, e /= (x,y), e /= (y,x)] | rs <- g ]