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

showv 10 = "T"
showv 11 = "J"
showv 12 = "Q"
showv 13 = "K"
showv 14 = "A"
showv n = show n

readv "T" = 10
readv "J" = 11
readv "Q" = 12
readv "K" = 13
readv "A" = 14
readv s = read s :: Int

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

comp line = (vs,cmp)
    where (a,b) = (splitAt 5) . map parse.words $ line
          vs = ((a,select a), (b,select b))
          cmp = select a `compare` select b

straightFlush :: [Card] -> [Rule]
straightFlush cards = if sameSuit cards
                      then map StraightFlush . cons $ cards
                      else []

fourKind cards = case stats cards of
                   [(4,x),(1,y)] -> [FourKind x y]
                   _ -> []

fullHouse cards = case stats cards of
                    [(3,x),(2,y)] -> [FullHouse x y]
                    _ -> []

flush cards = if sameSuit cards then [Flush] else []

straight :: [Card] -> [Rule]
straight = map Straight . cons

threeKind cards = case stats cards of
                     [(3,x),(1,y),(1,z)] -> [ThreeKind x y z]
                     _ -> []

twoPair cards = case stats cards of
                  ((2,x):(2,y):_) -> [TwoPair x y]
                  _  -> []

pair cards = case  stats cards of
               [(2,x),(1,y),(1,z),(1,w)] -> [Pair x y z w]
               _ -> []

stats cards = reverse . sort . map Data.Tuple.swap . M.toList $ stats
    where values = map cardValue cards
          stats = foldl (\m v -> M.insertWith (+) v 1 m) M.empty  values

highCard cards = [HighCard . maximum . map cardValue $ cards]

select cards = head . concatMap ($cards) $ [ straightFlush ,fourKind ,fullHouse ,flush ,straight ,threeKind ,twoPair ,pair ,highCard]

main = do
    c <- readFile "p054_poker.txt"
    let rounds = lines c
    let f = map comp $ rounds
    print (length . filter ((==GT).snd) $ f)
