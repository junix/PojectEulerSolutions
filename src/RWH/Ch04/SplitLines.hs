module RWH.Ch04.SplitLines where

splitLines :: String -> [String]
splitLines =
    go []
  where go :: [String] -> String -> [String]
        go xss []        = reverse xss
        go xss ('\n':xs) = go xss xs
        go xss ('\r':xs) = go xss xs
        go xss rx@(x:xs) = go (line:xss) remain
          where (line, remain) = break isLineTerm rx

isLineTerm :: Char -> Bool
isLineTerm x = x == '\n' || x == '\r'

splitLines' :: String -> [String]
splitLines' xs =
  case dropWhile isLineTerm xs of
    []  -> []
    xs' -> let (l,r) = break isLineTerm xs'
           in l:splitLines r

splitLines'' :: String -> [String]
splitLines'' s = snd.step.foldr go ([], []) $ s
  where
    go x acc@(xs, xss)
      | isLineTerm x = step acc
      | otherwise    = (x:xs, xss)
    step r@([], xss) = r
    step (xs, xss) = ([], xs:xss)

splitLines3 :: String -> [String]
splitLines3 [] = []
splitLines3 xs = if null l then [] else l:splitLines3 r
  where
    xs' = dropWhile isLineTerm xs
    (l, r) = break isLineTerm xs'
