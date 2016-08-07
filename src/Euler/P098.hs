module P098 where
import Data.List (delete, sortBy, sort,nub,partition,find,(\\))
import Data.Function (on)
import qualified Data.Map as M

type CharMap = M.Map Char Integer

main = readFile "./p098_words.txt" >>= print.euler

ws :: String -> [String]
ws s = read ('[':s ++ "]")

groupWords [] = []
groupWords (xs:xss) = (length xs , xs:filter (/=xs) mss):groupWords uss
    where cs = sort xs
          (mss, uss) = partition ((==cs).sort) xss

euler s = maximum .
          concatMap seek .
          map snd .
          reverse .
          sort .
          filter ((>1).length.snd) .
          groupWords .
          ws $ s

perm :: String -> [CharMap]
perm xs = map (M.fromList . zip cset) . go (length cset) $ [0..9]
    where cset = nub xs
          go 0 _  = [[]]
          go n cs = [c:ns | c <- cs, ns <- go (n-1) (delete c cs)]

seek css@(cs:_) = [ maximum vs | d <- ds, let vs = map (str2v d) css, all isSquare vs]
    where ds = perm cs

str2v :: CharMap -> String -> Integer
str2v dict cs@(c:_) = if (dict M.! c == 0) then 2 else go (map (dict M.!) cs) 0
    where go [] acc = acc
          go (d:ds) acc = go ds (acc*10+d)

isSquare n = r^2 == n
    where r = truncate . sqrt . fromInteger $ n
