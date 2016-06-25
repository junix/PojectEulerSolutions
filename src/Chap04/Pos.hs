module Chap02.Pos where

import Data.Maybe (listToMaybe)

pos :: (Eq a) =>a -> [a] -> Maybe Int
pos _ [] = Nothing
pos e (x:xs) = if e==x
               then Just 0
               else (+1) <$> tailPos
               where tailPos = pos e xs

pos' e = listToMaybe.map fst.filter (\(i,v) -> v == e) . zip [0..]

pos'' e xs= listToMaybe [ i | (i, e') <- zip [0..] xs, e' == e]

pos''' :: Eq a => a ->[a] -> Maybe Int
pos''' e xs= pos_aux e xs 0
        where
            pos_aux :: Eq a => a ->[a] -> Int -> Maybe Int
            pos_aux _ [] _ = Nothing
            pos_aux e (x:xs) index
                | e == x = Just (index + 1)
                | otherwise = pos_aux e xs (index+1)
