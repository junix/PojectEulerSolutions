module P191 where
import Data.List (nub)
import qualified Data.Map as M
{-
A particular school offers cash rewards to children with good attendance
and punctuality. If they are absent for three consecutive days or late on
more than one occasion then they forfeit their prize.

During an n-day period a trinary string is formed for each child
consisting of L's (late), O's (on time), and A's (absent).

Although there are eighty-one trinary strings for a 4-day period that can
be formed, exactly forty-three strings would lead to a prize:

OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA
OAOL OAAO OAAL OALO OALA OLOO OLOA OLAO OLAA AOOO
AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL
AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA
LAOO LAOA LAAO

How many "prize" strings exist over a 30-day period?
-}

data Att = L | O | A deriving (Eq,Ord,Show)
type Rule = ([Att], [Att])
type Dict = M.Map (Rule,Integer) Integer

nextR (_, ls)    O = ([A,A], ls)
nextR (_, _)     L = ([A,A], [])
nextR (_:xs, ls) A = (xs, ls)

nextC ([],[]) = [O]
nextC ([], _) = [O,L]
nextC (_, []) = [O,A]
nextC (_, _ ) = [O,L,A]

nextS :: Rule -> [Rule]
nextS r = map (nextR r) . nextC $ r

rules = [ (replicate ac A, replicate lc L) | ac <- [0,1,2], lc <- [0,1]]

update n dict = foldr (\r d -> M.insert r (count r (n-1)) d) dict rules
    where count r c
              | c < 0     = 1
              | otherwise = sum . map (dict M.!) . nextS $ r

steps n = go 0 (update 0 M.empty)
    where go x d
              | x >= n    = d
              | otherwise = let x' = x + 1
                            in  go x' (update x' d)

euler n = steps n M.! ([A,A],[L])