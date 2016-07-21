module Euler.P054 where
import Data.List (nub,sort,permutations)
import qualified Data.Map.Strict as M
import qualified Data.Tuple (swap)

data CardType = H | C | S | D deriving (Eq, Show, Read, Ord, Enum)
data Rule = HighCard Int
          | Pair Int Int Int Int
          | TwoPair Int Int
          | ThreeKind Int Int Int
          | Straight Int
          | Flush
          | FullHouse Int Int
          | StraightFlush Int
          | FourKind Int Int deriving (Eq,Ord,Show)

data Card = Card CardType Int

instance Eq (Card) where
    Card _ v1 == Card _ v2 = v1 == v2

instance Ord Card where
    Card _ v1 `compare` Card _ v2 = v1 `compare` v2

showv n = (:[]).(!!n) $ (['0'..'9'] ++ "TJQKA")

dict = M.fromList $ [("T", 10), ("J", 11), ("Q",12), ("K",13), ("A",14)] ++ map (\x -> (show x, x)) [1..9]
readv s = dict M.! s

instance Show Card where
    show (Card t v) = showv v ++ show t

cardType (Card t v) = t
cardValue (Card t v) = v

sameSuit :: [Card] -> Bool
sameSuit = (==1).length . nub . map cardType

cons :: [Card] -> [Int]
cons cards = if isCons then [head values] else []
    where values = sort . map cardValue $ cards
          isCons = all (==(-1)). zipWith (-) values $ (tail values)

parse [v,t] = Card (read [t]) (readv [v])

comp line = select' a `compare` select' b
    where (a,b) = (splitAt 5) . map parse.words $ line

stats cards = reverse . sort . map Data.Tuple.swap . M.toList $ stats
    where values = map cardValue cards
          stats = foldl (\m v -> M.insertWith (+) v 1 m) M.empty  values

select' xs
    | isSameSuit && length c > 0 = StraightFlush (head c)
    | freqCount == [4,1]         = FourKind (head items) (last items)
    | freqCount == [3,2]         = FullHouse (head items) (last items)
    | isSameSuit                 = Flush
    | length c > 0               = Straight (head c)
    | freqCount == [3,1,1]       = ThreeKind (items !! 0) (items !! 1) (items!!2)
    | freqCount == [2,2,1]       = TwoPair (items !! 0) (items !! 1)
    | freqCount == [2,1,1,1]     = Pair (items !! 0) (items !! 1) (items!!2) (items !! 3)
    | otherwise = HighCard . maximum . map cardValue $ xs
    where isSameSuit = sameSuit xs
          c          = cons xs
          freq       = stats xs
          freqCount  = map fst freq
          items      = map snd freq

main = do
    c <- readFile "p054_poker.txt"
    let winCount = length.filter (==GT). map comp . lines $ c
    print winCount
